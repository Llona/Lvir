library(tcltk2)
require(graphics); require(stats)

# ===draw all GUI component==
tkbutton <- tk2button
tkframe <- tk2frame
tklabel <- tk2label
tklistbox <- tk2listbox

component_list <- c("+", "-", "x", "/")

tclServiceMode(FALSE) # don't display until complete
top <- tktoplevel()
tktitle(top) <- "GUI test"

plotFont <- "Helvetica 18"

#listbox
listbox <- tklistbox(top, height = 50, selectmode = "single")
tkpack(listbox, side="left", fill="x", pady="2m")

for (component in component_list)
  tkinsert(listbox, "end", component)
tkselection.set(listbox, 0)

# button and button frame
#buttons <- tkframe(top)
#tkpack(buttons, side="bottom", fill="x", pady="2m")
#dismiss <- tkbutton(buttons, text="Dismiss",
#                      command=function()tkdestroy(top))
#tkpack(dismiss, side="left", expand=TRUE)

# canvas
canvas <- tkcanvas(top, relief="raised", width=600, height=800 )
tkpack(canvas, side="top", fill="both")

tkcreate(canvas, "text", 225, 20, text="LabView",
         font=plotFont, fill="brown")
#=======Draw GUI done======

plotDown <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  tkdtag(canvas, "selected")
  tkaddtag(canvas, "selected", "withtag", "current")
  tkitemraise(canvas,"current")
  lastX <<- x
  lastY <<- y
}

plotMove <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  tkmove(canvas, "selected", x - lastX, y - lastY)
  lastX <<- x
  lastY <<- y
}

plotComponent <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  componentChoice <- component_list[as.numeric(tkcurselection(listbox)) + 1]
  item <- tkcreate(canvas, "text", x, y, 
                   text=componentChoice,
                   font=plotFont, fill="SkyBlue2")
  tkaddtag(canvas, "point", "withtag", item)
  item
  count <<- count + 1
  cat("double click x:", count, "\n")
}
count <- 0
lastX <- 0
lastY <- 0

tkitembind(canvas, "point", "<Any-Enter>",
           function() tkitemconfigure(canvas, "current", fill="red"))
tkitembind(canvas, "point", "<Any-Leave>",
           function() tkitemconfigure(canvas, "current", fill="SkyBlue2"))
tkitembind(canvas, "point", "<1>", plotDown)
tkitembind(canvas, "point", "<ButtonRelease-1>",
           function(x){
             tkdtag(canvas, "selected")
           })
tkbind(canvas, "<B1-Motion>", plotMove)
# press shift + mouse left key
tkbind(canvas, "<Shift-Button-1>", plotComponent)
tclServiceMode(TRUE)
tkfocus(top)
