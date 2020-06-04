# have your data ready
x <- 1:10
y <- 1:10

# create the path and name to your new figure
# here the name is Test_image, type is .jpg
# width and height can be changed, they're in nb of pixels
jpeg("Figures/Test_image.jpg", width = 350, height = 350)
# make your plot
ggplot(data = final_dataset) +
	geom_jitter(aes(x = park.area.percentage, y = spc_richness, col=quarantine)) +
	theme_classic() + 
	labs(x="Percentage of park area", y="Species richness")
# close the plot to save it.
dev.off()
