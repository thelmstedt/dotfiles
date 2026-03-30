// component/SparkLine.qml — line chart for popup history displays
import QtQuick
import qs.service

Canvas {
    id: root

    implicitWidth: 200
    implicitHeight: 40

    property var values: []
    property real maxVal: 0        // 0 = auto-scale to max of values
    property color lineColor: Theme.label_bg

    onValuesChanged: requestPaint()
    onLineColorChanged: requestPaint()
    onWidthChanged: requestPaint()

    onPaint: {
        const ctx = getContext("2d")
        ctx.clearRect(0, 0, width, height)

        const pts = values
        if (pts.length < 2) return

        let max = maxVal
        if (max <= 0) {
            for (let i = 0; i < pts.length; i++) if (pts[i] > max) max = pts[i]
        }
        if (max <= 0) return

        const n   = pts.length
        const px  = i => (i / (n - 1)) * width
        const py  = v => height - (v / max) * (height - 2) - 1

        // Fill under line
        ctx.beginPath()
        ctx.moveTo(px(0), py(pts[0]))
        for (let i = 1; i < n; i++) ctx.lineTo(px(i), py(pts[i]))
        ctx.lineTo(px(n - 1), height)
        ctx.lineTo(0, height)
        ctx.closePath()
        ctx.fillStyle = Qt.rgba(lineColor.r, lineColor.g, lineColor.b, 0.15)
        ctx.fill()

        // Line
        ctx.beginPath()
        ctx.moveTo(px(0), py(pts[0]))
        for (let i = 1; i < n; i++) ctx.lineTo(px(i), py(pts[i]))
        ctx.strokeStyle = lineColor
        ctx.lineWidth = 1.5
        ctx.lineJoin = "round"
        ctx.lineCap = "round"
        ctx.stroke()
    }
}
