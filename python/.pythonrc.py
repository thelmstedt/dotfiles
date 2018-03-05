# from https://gist.github.com/760273

import os, sys
# Coloured prompt
if os.getenv('TERM') in ('xterm', 'vt100', 'rxvt', 'Eterm', 'putty', 'rxvt-unicode-256color'):

    try:
        import readline
    except ImportError:
        sys.ps1 = '\033[0;32m>>> \033[0m'
        sys.ps2 = '\033[0;32m... \033[0m'
    else:
        sys.ps1 = '\001\033[0;32m\002>>> \001\033[0m\002'
        sys.ps2 = '\001\033[0;32m\002... \001\033[0m\002'

# Completion!
try:
    import readline
except ImportError:
    print("Module readline not available.")
else:
    # persistent history
    histfile = os.path.expanduser('~/.pythonhistory')
    try:
        readline.read_history_file(histfile)
    except IOError:
        pass
    import atexit
    atexit.register(readline.write_history_file, histfile)
    del histfile, atexit

    # tab completion
    try:
        sys.path.append(os.path.join(os.getenv('HOME'), 'src', 'rlcompleter2'))
        import rlcompleter2
        rlcompleter2.setup()
        del rlcompleter2
    except ImportError:
        import rlcompleter
        readline.parse_and_bind("tab: complete")
        del rlcompleter
    del readline


del sys
del os
