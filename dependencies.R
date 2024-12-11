# dependencies.R
suppressWarnings({
    # Cargar librerías necesarias
    library(ggplot2)

    # Función para ploteo de boxplot
    boxplot <- function(df) {
        for (col in colnames(df)) {
            p <- ggplot(df, aes_string(x = "1", y = col)) + 
                geom_boxplot() + 
                labs(title = paste("Boxplot de", col), x = "", y = col) +
                theme_minimal()
            print(p)
        }
    }

})
