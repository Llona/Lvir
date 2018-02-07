library(tcltk2)
require(graphics); require(stats)

# ===draw all GUI component==
tkbutton <- tk2button
tkframe <- tk2frame
tklabel <- tk2label
tklistbox <- tk2listbox

component_list_gv <- c("+", "-", "x", "/", "input")

last_x_gi <- 0
last_y_gi <- 0
comp_gui_index_gl <- list()
plot_font_gs <- "Helvetica 18"

# ====Start to draw GUI====
tclServiceMode(FALSE) # don't display until complete
top <- tktoplevel()
tktitle(top) <- "GUI test"

#listbox
listbox <- tklistbox(top, height = 50, selectmode = "single")
tkpack(listbox, side="left", fill="x", pady="2m")

for (component in component_list_gv)
  tkinsert(listbox, "end", component)
tkselection.set(listbox, 0)

# button and button frame
#buttons <- tkframe(top)
#tkpack(buttons, side="bottom", fill="x", pady="2m")
#dismiss <- tkbutton(buttons, text="Dismiss",
#                      command=function()tkdestroy(top))
#tkpack(dismiss, side="left", expand=TRUE)

# canvas
canvas <- tkcanvas(top, relief="raised", width=600, height=860, bg='#dddddd' )
tkpack(canvas, side="top", fill="both")

tkcreate(canvas, "text", 225, 20, text="LabView",
         font=plot_font_gs, fill="brown")
#=======Draw GUI done======

plotDown <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  tkdtag(canvas, "selected")
  tkaddtag(canvas, "selected", "withtag", "current")
  tkitemraise(canvas,"current")
  last_x_gi <<- x
  last_y_gi <<- y
}

plotMove <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  tkmove(canvas, "selected", x - last_x_gi, y - last_y_gi)
  last_x_gi <<- x
  last_y_gi <<- y
}

plotComponent <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  comp_listbox_choice_ls <- component_list_gv[as.numeric(tkcurselection(listbox)) + 1]
  item <- tkcreate(canvas, "text", x, y, 
                   text=comp_listbox_choice_ls,
                   font=plot_font_gs, fill="SkyBlue2")
  tkaddtag(canvas, "component", "withtag", item)
  item
  
  # -- item save to list--
  tkaddtag(canvas, "new", "withtag", item)
  component_id_ls <- tkfind(canvas, "withtag", "new")
  tkdtag(canvas, "new")
  
  cat("create item:", as.numeric(component_id_ls),"\n")
  comp_ls = paste(comp_listbox_choice_ls,"_", sep='')
  comp_ls = paste(comp_ls,as.character(component_id_ls), sep='')
  comp_gui_index_gl[[as.character(component_id_ls)]] <<- comp_ls
  
  
  #for(i in attributes(comp_gui_index_gl)){  
  #for(i in comp_gui_index_gl){  
  #  cat("list:", i)
  #}  

}

getGuiSelectCompIndex <- function(x, y) {
  component_id_ls <- tkfind(canvas, "withtag", "current")
  cat(as.numeric(component_id_ls), "\n")
  cat(comp_gui_index_gl[[as.character(component_id_ls)]], "\n")
}



tkitembind(canvas, "component", "<Any-Enter>",
           function() tkitemconfigure(canvas, "current", fill="red"))
tkitembind(canvas, "component", "<Any-Leave>",
           function() tkitemconfigure(canvas, "current", fill="Blue"))
tkitembind(canvas, "component", "<1>", plotDown)
tkitembind(canvas, "component", "<ButtonRelease-1>",
           function(x){
             tkdtag(canvas, "selected")
           })
tkbind(canvas, "<B1-Motion>", plotMove)
# press shift + mouse left key
tkbind(canvas, "<Shift-Button-1>", plotComponent)
tkitembind(canvas, "component", "<Double-Button-1>", getGuiSelectCompIndex)

tclServiceMode(TRUE)
tkfocus(top)
