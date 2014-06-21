

getButtons <- function(e)
{
    if (is.null(e$buttons)) NULL
    else
    {
        x <- e$buttons()
        ## as.logical() loses names
        0 != c(left = Qt$Qt$LeftButton & x,
               right = Qt$Qt$RightButton & x,
               middle = Qt$Qt$MidButton & x)
    }
}

getModifiers <- function(e)
{
    if (is.null(e$modifiers)) NULL
    else
    {
        x <- e$modifiers()
        0 != c(shift = Qt$Qt$ShiftModifier & x,
               control = Qt$Qt$ControlModifier & x,
               alt = Qt$Qt$AltModifier & x,
               meta = Qt$Qt$MetaModifier & x,
               keypad = Qt$Qt$KeypadModifier & x,
               groupswitch = Qt$Qt$GroupSwitchModifier & x)
    }
}


parseQtEvent <- function(e, ...)
{
    UseMethod("parseQtEvent")
}

parseQtEvent.QGraphicsSceneEvent <- function(e, id = "", ...)
{
    parseMouseEvent(e, id)
}

parseQtEvent.QGraphicsSceneWheelEvent <- function(e, id = "", ...)
{
    parseWheelEvent(e, id)
}

parseQtEvent.QKeyEvent <- function(e, id = "", ...)
{
    parseKeyEvent(e, id)
}



parseMouseEvent <- function(e, id = "")
{
    list(id = id,
         type = e$type(),
         scenePos = as.numeric(e$scenePos()),
         lastScenePos = as.numeric(e$lastScenePos()),
         ## buttonDownScenePos = as.numeric(e$buttonDownScenePos()),
         buttons = getButtons(e),
         modifiers = getModifiers(e))
}

parseWheelEvent <- function(e, id = "")
{
    list(id = id,
         type = e$type(),
         scenePos = as.numeric(e$scenePos()),
         delta = as.integer(e$delta()),
         vertical = e$orientation() == Qt$Qt$Horizontal,
         ## buttonDownScenePos = as.numeric(e$buttonDownScenePos()),
         buttons = getButtons(e),
         modifiers = getModifiers(e))
}

parseKeyEvent <- function(e, id = "")
{
    list(id = id,
         type = e$type(),
         count = as.integer(e$count()),
         autorepeat = e$isAutoRepeat(),
         key = as.integer(e$key()), # enum Qt::Key
         text = as.character(e$text()),
         modifiers = getModifiers(e))
}

