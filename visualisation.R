#library(plotly)

magic_uscore <- function(...) {
	args_in <- list(...)
	res <- list()
	for (var in names(args_in)) {
		var_name_split <- strsplit(var, '_')[[1]]
		val <- args_in[[var]]		
		if (length(var_name_split) == 1) {
			e <- glue::glue('list({var} = {val})', var = var, val = val)
		} else if (length(var_name_split) > 1) {
			beginning <- 'list('
			if (is.character(val)) {
				middle <- glue::glue("{mid} = '{val}'", 
							  mid = paste0(var_name_split, collapse = ' = list('),
						 	  val = val)
			} else {
				middle <- glue::glue("{mid} = {val}", 
								  mid = paste0(var_name_split, collapse = ' = list('),
								  val = val)
			}
			ending <- strrep(')',length(var_name_split))
			e <- paste0(beginning, middle, ending)
		}
	res <- c(res, e)
	}
	res
}

#' This function uses the argument `data_list` and each item is passed onto the 
#' `data` argument of `plotly::layout`
#' 
magic_layout <- function(p, ..., data_list = NULL) {
	args_list <- magic_uscore(...)
	for (i in seq_along(args_list)) {
		arg <- args_list[[i]]
		data <- data_list[[i]]
		# convert to list from text
		#print(arg)
		listed_arg <- eval(parse(text = arg))
		# combine the plot and the listed arg in one list
		listed_args <- c(list(p = p), listed_arg, data = data)
		class(listed_args) <- "plotly"
		#print(str(listed_args))
		p <- do.call(plotly::layout, listed_args)
	}
	p
}

df <- tibble::tibble(x = 1:10, y = 10:1)
plt <- plot_ly(data = df) %>%
	add_lines(x = ~x, y = ~y)

# these two lines are the same
#plt %>% magic_layout(title_text = "<b>Title Here</b>", xaxis_title_text = 'Axis Title Here')
#plt %>% layout(title = list(text = "<b>Title Here</b>"),
#			   xaxis = list(title = list(text = 'Axis Title Here')))

