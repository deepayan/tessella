
initApp <- function(template.file = system.file("resources/pixi.html.template",
                                                package = "tessella.libuv"))
{
    template <- paste(readLines(template.file), collapse = "\r\n")
    wsock <- FALSE # will be used to access $send() later
    lastMessage <- NULL
    call <- function(req)
    {
        wsUrl <- sprintf("'ws://%s'", ifelse(is.null(req$HTTP_HOST),
                                             req$SERVER_NAME, req$HTTP_HOST))
        list(status = 200L,
             headers = list('Content-Type' = 'text/html'),
             body = sprintf(template, wsUrl))
    }
    onWSOpen <- function(ws)
    {
        wsock <<- ws
        ws$onMessage(
            function(binary, msg)
            {
                lastMessage <<- msg
            })
    }
    environment()
}

