library(ggplot2)
library(stargazer)

data <- read.csv('../data/who_not_what.csv')

data$report <- data$report_up
data[data$report_data == 1, 'report'] <- 0

IVs <- c("Extreme", "Attack.Govt.", "User", "Rumors", "Retweets", "Mobilizing", "Political.Humor", "GOV_CR", "COR_LO", "COL")
IV_names <- c("Extreme", "Govt. Attack", "User Attributes", "Rumors", "Retweets", "Mobilizing", "Political Humor", "Govt. Criticism", "Local Govt. Corruption", "Collective Action")
types <- c(rep("Instructions", 7), rep("Content", 3))

mod <- glm(report ~ Extreme + Attack.Govt. + User + Rumors + Retweets + Mobilizing + Political.Humor + GOV_CR + COR_LO + COL, family=binomial(link='logit'), data=data)

## Save TeX table

title <- "Logistic Regression Model for ``Reporting Up''"
output <- capture.output(stargazer(mod, title=title, header=FALSE, omit = c("Constant", "(Intercept)"), table.placement = "H", omit.stat=c("rsq", "adj.rsq", "f", "ser", "ll", "aic"),dep.var.labels.include=FALSE, model.names=FALSE, covariate.labels=IV_names, column.labels=c('Report Up'), no.space=TRUE))
output <- gsub("\\begin{tabular}", "\\resizebox{!}{.35\\paperheight}{\\begin{tabular}", output, fixed=TRUE)
output <- gsub("\\end{tabular}", "\\end{tabular}}", output, fixed=TRUE)
cat(output, file="../tables/reg_model.tex", sep="\n")

## Coefficient Plot

plot_df <- data.frame()
conf <- confint(mod, level = 0.95)
for (i in 1:length(IVs)) {
	b <- coef(summary(mod))[IVs[i],][[1]]
	ci <- conf[IVs[i], ]
	vals <- data.frame(
		beta = b,
		ci_bot = ci[[1]],
		ci_top = ci[[2]],
		type = types[i],
		iv = IV_names[i]
	)
	plot_df <- rbind(plot_df, vals)
}

plot_df$iv <- factor(plot_df$iv, levels = rev(c("Govt. Criticism", "Local Govt. Corruption", "Collective Action", "User Attributes", "Govt. Attack", "Political Humor", "Extreme",  "Rumors", "Retweets", "Mobilizing")))

cbPalette <- c("#000000", "#009E73", "#E79F00", "#0072B2", "#D55E00", "#CC79A7", "#F0E442", "#9AD0F3", "#FFFFFF")
shapes <- c(19, 17, 15, 18, 0, 1, 2, 3, 4)

ggplot(plot_df, aes(x=iv, y=beta, color=type)) +
	geom_point(size = 2, position = position_dodge(0.9), stat="identity") +
	geom_errorbar(aes(ymin = ci_bot, ymax = ci_top, color = type), position = position_dodge(0.9), width = 0) +
	xlab("") +
	ylab("Coefficient Estimate") +
	geom_hline(yintercept = 0, color = gray(1/2), lty = 2) +
	scale_color_manual(values=cbPalette) +
	scale_shape_manual(values = shapes) + 
	coord_flip()+
	theme_minimal() +
	theme(plot.title = element_text(face="bold"),
			legend.position = c(.8, .85),
			legend.justification = c(0, 0), 
			legend.background = element_rect(colour="grey80"),
			legend.title = element_blank())

ggsave("../figures/coef_plot.pdf", width = 7.5, height = 5)