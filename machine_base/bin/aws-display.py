import boto3
import click
import time
from datetime import datetime, timezone
from rich.console import Console
from rich.table import Table
import re
from typing import Optional, List
from rich.rule import Rule
from rich.theme import Theme


##
## Fetching data from AWS
##

def get_ecs_details():
    ecs = boto3.client('ecs')
    containers = []

    for cluster in ecs.list_clusters()['clusterArns']:
        cluster_name = cluster.split('/')[-1]
        tasks = (ecs.list_tasks(cluster=cluster, desiredStatus='STOPPED')['taskArns'] +
                 ecs.list_tasks(cluster=cluster, desiredStatus='RUNNING')['taskArns'] +
                 ecs.list_tasks(cluster=cluster, desiredStatus='PENDING')['taskArns'])

        if not tasks:
            continue

        for task in ecs.describe_tasks(cluster=cluster, tasks=tasks)['tasks']:
            start_time = task.get('startedAt')
            stopCode = task.get('stopCode')
            for container in task['containers']:
                containers.append({
                    'cluster': cluster_name,
                    'task_id': task['taskArn'].split('/')[-1],
                    'name': container['name'],
                    'status': format_status(container['lastStatus']),
                    'stop_code': format_stop_code(stopCode),
                    'image': container.get('image', ''),
                    'ip': container.get('networkInterfaces', [])[0].get('privateIpv4Address', '') if container.get(
                        'networkInterfaces') else '',
                    'cpu': container.get('cpu', ''),
                    'memory': container.get('memory', ''),
                    'started': start_time.strftime(fmt) if start_time else '',
                    'start_time': start_time.strftime(fmt) if start_time else datetime.now().strftime(fmt),
                    'uptime': format_duration(start_time)
                })

    containers.sort(key=lambda x: ('nonprod' in x['cluster'], x['cluster'], x['name']))
    headers = ['cluster', 'name', 'task_id', 'image', 'ip', 'cpu', 'memory', 'started', 'uptime', 'status', 'stop_code']
    return headers, containers


def get_ssm_params():
    ssm = boto3.client('ssm')
    params = []
    paginator = ssm.get_paginator('get_parameters_by_path')

    for page in paginator.paginate(Path='/', Recursive=True, WithDecryption=True):
        for param in page['Parameters']:
            params.append({
                'name': param['Name'],
                'value': param['Value'],
                'type': param['Type'],
                'modified': param['LastModifiedDate'].strftime('%Y-%m-%d %H:%M:%S')
            })

    return ['name', 'type', 'modified', 'value'], sorted(params, key=lambda x: x['name'])


def get_secrets():
    sm = boto3.client('secretsmanager')
    secrets = []

    for secret in sm.list_secrets()['SecretList']:
        try:
            value = sm.get_secret_value(SecretId=secret['Name'])['SecretString']
        except Exception:
            value = ''
        secrets.append({
            'name': secret['Name'],
            'value': value,
            'modified': secret['LastChangedDate'].strftime(fmt) if 'LastChangedDate' in secret else 'n/a',
            'description': secret.get('Description', 'n/a')
        })

    secrets.sort(key=lambda x: x['name'])
    headers = ['name', 'modified', 'description', 'value']
    return headers, secrets

def get_ec2():
    ec2 = boto3.client('ec2')
    instances = []

    for reservation in ec2.describe_instances()['Reservations']:
        for instance in reservation['Instances']:
            name_tag = next((tag['Value'] for tag in instance.get('Tags', []) if tag['Key'] == 'Name'), '')
            instances.append({
                'name': name_tag,
                'id': instance['InstanceId'],
                'type': instance['InstanceType'],
                'state': format_status(instance['State']['Name']),
                'launch_time': instance['LaunchTime'].strftime(fmt),
                'vpc': instance.get('VpcId', ''),
                'subnet': instance.get('SubnetId', ''),
                'private_ip': instance.get('PrivateIpAddress', ''),
                'public_ip': instance.get('PublicIpAddress', ''),
                'key_pair': instance.get('KeyName', ''),
                'arch': instance['Architecture'],
                'reservation': wrap_colour('spot', 'blue') if 'SpotInstanceRequestId' in instance else 'ondemand',
            })

    return ['name', 'id', 'type', 'private_ip', 'public_ip', 'key_pair', 'vpc', 'subnet',  'reservation', 'launch_time', 'state'], sorted(instances, key=lambda x: x['name'])

def get_eks():
    eks = boto3.client('eks')
    clusters = []

    for cluster in eks.list_clusters()['clusters']:
        details = eks.describe_cluster(name=cluster)['cluster']
        clusters.append({
            'name': cluster,
            'version': details['version'],
            'status': format_status(details['status']),
            'created': details['createdAt'].strftime(fmt),
            'vpc': details['resourcesVpcConfig']['vpcId'],
            'endpoint': details['endpoint'],
            'role': details['roleArn'].split('/')[-1],
            'sg': details['resourcesVpcConfig']['clusterSecurityGroupId']
        })

    return ['name', 'version', 'status', 'created', 'vpc', 'endpoint', 'role', 'sg'], sorted(clusters, key=lambda x: x['name'])

def get_rds():
    rds = boto3.client('rds')
    instances = []

    for db in rds.describe_db_instances()['DBInstances']:
        instances.append({
            'id': db['DBInstanceIdentifier'],
            'engine': f"{db['Engine']} {db['EngineVersion']}",
            'class': db['DBInstanceClass'],
            'status': format_status(db['DBInstanceStatus']),
            'storage': f"{db['AllocatedStorage']} GB",
            'endpoint': db.get('Endpoint', {}).get('Address', ''),
            'port': str(db.get('Endpoint', {}).get('Port', '')),
            'multi_az': 'Yes' if db.get('MultiAZ', False) else 'No',
            'created': db['InstanceCreateTime'].strftime(fmt),
            'vpc': db.get('DBSubnetGroup', {}).get('VpcId', ''),
        })

    return ['id', 'engine', 'class', 'status', 'storage', 'endpoint', 'port', 'multi_az', 'vpc', 'created'], sorted(instances, key=lambda x: x['id'])

def get_lambda():
    lam = boto3.client('lambda')
    functions = []
    paginator = lam.get_paginator('list_functions')

    for page in paginator.paginate():
        for func in page['Functions']:
            last_modified = func['LastModified'].replace('T', ' ').replace('Z', '')
            runtime = func.get('Runtime', 'custom')
            memory = func.get('MemorySize', 0)
            timeout = func.get('Timeout', 0)

            functions.append({
                'name': func['FunctionName'],
                'runtime': runtime,
                'memory': f"{memory} MB",
                'timeout': f"{timeout}s",
                'handler': func.get('Handler', 'n/a'),
                'size': f"{func.get('CodeSize', 0) // (1024*1024)} MB",
                'modified': last_modified,
                'state': format_status(func.get('State', 'Unknown')),
                'description': func.get('Description', '')
            })

    return ['name', 'runtime', 'memory', 'timeout', 'handler', 'size', 'modified', 'state', 'description'], sorted(functions, key=lambda x: x['name'])

##
## Formatting
##

fmt = '%Y-%m-%d %H:%M:%S'


def wrap_colour(x: str, colour: str):
    return f'[{colour}]{x}[/{colour}]'


def format_status(status: str):
    colors = {
        'running': 'green',
        'pending': 'yellow',
        'stopped': 'red',
        'healthy': 'green',
        'active': 'green',
        'unhealthy': 'red',
    }
    c = colors.get(status.lower(), "white")
    return wrap_colour(status, c)


def format_stop_code(stopCode):
    if not stopCode:
        return ''
    elif stopCode == 'SpotInterruption':
        return wrap_colour(stopCode, 'green')
    else:
        return wrap_colour(stopCode, 'red')


def format_duration(start_time):
    if not start_time:
        return ''

    now = datetime.now(timezone.utc)
    duration = now - start_time

    days = duration.days
    hours = duration.seconds // 3600
    minutes = (duration.seconds % 3600) // 60

    if days > 0:
        return wrap_colour(f"{days}d {hours}h", 'green')
    elif hours > 0:
        return wrap_colour(f"{hours}h {minutes}m", "blue")
    else:
        return wrap_colour(f"{minutes}m", "purple")


def print_table(headers, data, show_lines=False, filter: Optional[List[str]] = None, expanded=False, watch=False):
    if not expanded:
        console = Console()
        if watch:
            console.clear()
        table = Table(show_lines=show_lines)
        for header in headers:
            table.add_column(header)

        for d in data:
            row = []
            matched = match_filters(d, filter, headers)

            if not matched:
                continue
            for h in headers:
                val = d[h]
                if filter:
                    for f in filter:
                        val = re.sub(f'({re.escape(f)})', r'[green]\1[/green]', val, flags=re.IGNORECASE)
                row.append(val)
            table.add_row(*row)
        console.print(table)
    else:
        console = Console(theme=Theme({"key": "blue bold"}))
        if watch:
            console.clear()
        for i, d in enumerate(data):
            matched = match_filters(d, filter, headers)
            if matched:
                for h in headers:
                    val = d[h]
                    if filter:
                        for f in filter:
                            val = re.sub(f'({re.escape(f)})', r'[green]\1[/green]', val, flags=re.IGNORECASE)
                    console.print(f"[key]{h}[/key]:\n{val}", highlight=False)
                if i != len(data) -1:
                    console.print("")
                    console.print(Rule())
                    console.print("")


def match_filters(d, filter, headers):
    if filter is None:
        matched = True
    else:
        matched = all([any([f.lower() in d[h1].lower() for h1 in headers]) for f in filter])
    return matched


##
## main
##

def get_data(command):
    if command == "ecs":
        headers, data = get_ecs_details()
    elif command == "ssm":
        headers, data = get_ssm_params()
    elif command == "secrets":
        headers, data = get_secrets()
    elif command == "ec2":
        headers, data = get_ec2()
    elif command == "eks":
        headers, data = get_eks()
    elif command == "rds":
        headers, data = get_rds()
    elif command == "lambda":
        headers, data = get_lambda()
    else:
        raise Exception("impossible")
    show_lines = command in ["secrets", "ssm"]
    return data, headers, show_lines


@click.command()
@click.argument('command', type=click.Choice(['ecs', 'ssm', 'secrets', 'ec2', 'eks', 'rds', "lambda"]))
@click.option('--watch', is_flag=True, help='Watch mode with continuous updates')
@click.option('--interval', default=5, help='Refresh interval in seconds')
@click.option('--filter', multiple=True, help='Filter rows where any column contains values')
@click.option('--expanded', '-x', is_flag=True, help='Expanded display mode')
def main(command, watch, interval, filter, expanded):
    while True:
        data, headers, show_lines = get_data(command)
        print_table(headers, data, show_lines=show_lines, filter=list(filter), expanded=expanded, watch=watch)

        if not watch:
            break
        print(f"\nRefreshing every {interval} seconds. Press Ctrl+C to exit.")
        print(f"Refreshed at {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        time.sleep(interval)


# [Previous code remains unchanged]
if __name__ == '__main__':
    main()
