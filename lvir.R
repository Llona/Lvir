library(tcltk2)
library(xml2)
#require(graphics); require(stats)


tkbutton <- tk2button
tkframe <- tk2frame
tklabel <- tk2label
tklistbox <- tk2listbox

ITEM_NAME_INDEX <- 1
COMP_NAME_INDEX <- 2


last_x_gi <- 0
last_y_gi <- 0
plot_font_gs <- "Helvetica 18"
#comp_list_gv <- c("+", "-", "x", "/", "input")
comp_list_gv <- c()

item_prop_gl <<- list()
item_filename_gl <<- list()
comp_gui_index_gl <<- list()
comp_prop_gl <<- list()

# todo: use item base for develop, need implement select item function
item_current_num_gv <<- 1
item_current_neme_gv <<- ""

ReadSettingToList <- function(file_path) 
{
     xml_contents_ll <- read_xml(file_path)
     # ---find all item---
     xml_item_ll <- xml_find_all(xml_contents_ll, ".//item")
     for(item_content in xml_item_ll)
     {
          item_name_ls <- xml_attr(item_content, "id")
          
          # store item file name to item_filename_gl
          xml_item_filename_ll <- xml_text(xml_find_all(item_content, ".//filename"))
          item_filename_gl[[item_name_ls]] <<- xml_item_filename_ll[[1]]
          
          # ---store item prop to item_prop_gl---
          xml_comp_contents_ll <- xml_find_all(item_content, ".//component")
          comp_prop_ll <- list()
          # find all comp
          for (comp_content in xml_comp_contents_ll)
          {
               prop_ll <- list()
               comp_name_ls <- xml_attr(comp_content, "id")
               prop_ll[['in_num']] <- xml_text(xml_find_all(comp_content, ".//in_num"))
               prop_ll[['out_num']] <- xml_text(xml_find_all(comp_content, ".//out_num"))
               comp_prop_ll[[comp_name_ls]] <- prop_ll
          }
          item_prop_gl[[item_name_ls]] <<- comp_prop_ll
     }
}

PlotDown <- function(x, y) 
{
     x <- as.numeric(x)
     y <- as.numeric(y)
     tkdtag(canvas, "selected")
     tkaddtag(canvas, "selected", "withtag", "current")
     tkitemraise(canvas,"current")
     last_x_gi <<- x
     last_y_gi <<- y
}

PlotMove <- function(x, y) 
{
     x <- as.numeric(x)
     y <- as.numeric(y)
     tkmove(canvas, "selected", x - last_x_gi, y - last_y_gi)
     last_x_gi <<- x
     last_y_gi <<- y
}

PlotComponent <- function(x, y) 
{
     x <- as.numeric(x)
     y <- as.numeric(y)
     comp_listbox_choice_ls <- comp_list_gv[as.numeric(tkcurselection(listbox)) + 1]
     item <- tkcreate(canvas, "text", x, y, 
                      text=comp_listbox_choice_ls,
                      font=plot_font_gs, fill="SkyBlue2")
     tkaddtag(canvas, "component", "withtag", item)
     item
  
     # -- item save to list--
     tkaddtag(canvas, "new", "withtag", item)
     comp_id_ls <- tkfind(canvas, "withtag", "new")
     tkdtag(canvas, "new")
     
     # store gui index and component list
     cat("create item:", as.numeric(comp_id_ls),"\n")
     comp_ls = paste(comp_listbox_choice_ls,";;", sep='')
     comp_ls = paste(comp_ls,as.character(comp_id_ls), sep='')
     comp_ls = c(item_current_neme_gv, comp_ls)
     comp_gui_index_gl[[as.character(comp_id_ls)]] <<- comp_ls
     
     # store all component prop in gui
     comp_prop_gl[[comp_ls[COMP_NAME_INDEX]]] <<- item_prop_gl[[item_current_neme_gv]][[comp_listbox_choice_ls]]
     print(comp_prop_gl)

     #for(i in attributes(comp_gui_index_gl)){  
     #for(i in comp_gui_index_gl){  
     #  cat("list:", i)
     #}  
}

GetGuiSelectCompIndex <- function(x, y) 
{
     comp_id_ls <- tkfind(canvas, "withtag", "current")
     
     item_name_ll <- comp_gui_index_gl[[as.character(comp_id_ls)]][ITEM_NAME_INDEX]
     comp_name_ll <- comp_gui_index_gl[[as.character(comp_id_ls)]][COMP_NAME_INDEX]
     
     cat("GUI number:",as.numeric(comp_id_ls), "\n")
     cat("item name:", item_name_ll, "\n")
     cat("comp name:", comp_name_ll, "\n")
     print(comp_prop_gl[[comp_name_ll]])
}


# ---Read setting.xml and grnerate all item list---
ReadSettingToList("Settings\\settings.xml")
#print(item_prop_gl)
print(item_filename_gl)

# ---generate comp list---

comp_content_ll <- item_prop_gl[[item_current_num_gv]]

for (comp_name in names(comp_content_ll)) 
{
     comp_list_gv <<- c(comp_list_gv, c(comp_name))
}

item_current_neme_gv <<- names(item_prop_gl[item_current_num_gv])
# cat(item_current_neme_gv)

# ---start to draw GUI---
tclServiceMode(FALSE) # don't display until complete
top <- tktoplevel()
tktitle(top) <- "GUI test"

# listbox
listbox <- tklistbox(top, height = 50, selectmode = "single")
tkpack(listbox, side="left", fill="x", pady="2m")

for (component in comp_list_gv)
  tkinsert(listbox, "end", component)
tkselection.set(listbox, 0)

# canvas
canvas <- tkcanvas(top, relief="raised", width=600, height=860, bg='#dddddd' )
tkpack(canvas, side="top", fill="both")

tkcreate(canvas, "text", 225, 20, text="LabView",
         font=plot_font_gs, fill="brown")

# button and button frame
#buttons <- tkframe(top)
#tkpack(buttons, side="bottom", fill="x", pady="2m")
#dismiss <- tkbutton(buttons, text="Dismiss",
#                      command=function()tkdestroy(top))
#tkpack(dismiss, side="left", expand=TRUE)

# ---key bind---
tkitembind(canvas, "component", "<Any-Enter>",
           function() tkitemconfigure(canvas, "current", fill="red"))
tkitembind(canvas, "component", "<Any-Leave>",
           function() tkitemconfigure(canvas, "current", fill="Blue"))
tkitembind(canvas, "component", "<1>", PlotDown)
tkitembind(canvas, "component", "<ButtonRelease-1>",
           function(x){
             tkdtag(canvas, "selected")
           })
tkbind(canvas, "<B1-Motion>", PlotMove)
# press shift + mouse left key
tkbind(canvas, "<Shift-Button-1>", PlotComponent)
tkitembind(canvas, "component", "<Double-Button-1>", GetGuiSelectCompIndex)

# ---start GUI and set focuse---
tclServiceMode(TRUE)
tkfocus(top)
