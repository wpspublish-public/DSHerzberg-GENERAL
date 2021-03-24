suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(randomizr))
suppressMessages(library(blockTools))

# Load built-in dataset
data(HairEyeColor)
HairEyeColor <- data.frame(HairEyeColor)

# Transform so each row is a subject
# Columns describe subject's hair color, eye color, and gender
hec <- HairEyeColor[rep(1:nrow(HairEyeColor),
                        times = HairEyeColor$Freq), 1:3]

N <- nrow(hec)

# Fix the rownames
rownames(hec) <- NULL

# Set a seed for reproducability
set.seed(343)

# Create untreated and treated outcomes for all subjects
hec <- within(hec,{
  Y0 <- rnorm(n = N,mean = (2*as.numeric(Hair) + -4*as.numeric(Eye) + -6*as.numeric(Sex)), sd = 5)
  Y1 <- Y0 + 6*as.numeric(Hair) + 4*as.numeric(Eye) + 2*as.numeric(Sex)
})

# Calculate true ATE
with(hec, mean(Y1 - Y0))

# simple random assignment

Z <- simple_ra(N = N)
table(Z)

Z <- simple_ra(N = N, prob = 0.30)
table(Z)

Z <- simple_ra(N = N, num_arms = 3)
table(Z)

Z <- simple_ra(N = N, prob_each = c(.2, .2, .6))
table(Z)

Z <- simple_ra(N = N, prob_each = c(.2, .2, .6),
               conditions = c("control", "placebo", "treatment"))
table(Z)

# complete random assignment

Z <- complete_ra(N = N)
table(Z)

Z <- complete_ra(N = N, m = 200)
table(Z)

Z <- complete_ra(N = N, num_arms = 3)
table(Z)

Z <- complete_ra(N = N, m_each = c(100, 200, 292))
table(Z)

Z <- complete_ra(N = N, m_each = c(100, 200, 292),
                 conditions = c("control", "placebo", "treatment"))
table(Z)

sims <- 1000

# Set up empty vectors to collect results
simple_ests <- rep(NA, sims)
complete_ests <- rep(NA, sims)

# Loop through simulation 2000 times
for(i in 1:sims){
  hec <- within(hec,{
    
    # Conduct both kinds of random assignment
    Z_simple <- simple_ra(N = N)
    Z_complete <- complete_ra(N = N)
    
    # Reveal observed potential outcomes
    Y_simple <- Y1*Z_simple + Y0*(1-Z_simple)
    Y_complete <- Y1*Z_complete + Y0*(1-Z_complete)
  })
  
  # Estimate ATE under both models
  fit_simple <- lm(Y_simple ~ Z_simple, data=hec)
  fit_complete <- lm(Y_complete ~ Z_complete, data=hec)
  
  # Save the estimates
  simple_ests[i] <- coef(fit_simple)[2]
  complete_ests[i] <- coef(fit_complete)[2]
}

# block random assignment

Z <- block_ra(blocks = hec$Hair)
table(Z, hec$Hair)

Z <- block_ra(blocks = hec$Hair, num_arms = 3)
table(Z, hec$Hair)

Z <- block_ra(blocks = hec$Hair, conditions = c("Control", "Placebo", "Treatment"))
table(Z, hec$Hair)

Z <- block_ra(blocks = hec$Hair, prob_each = c(.3, .7))
table(Z, hec$Hair)

sort(unique(hec$Hair))
block_m_each <- rbind(c(78, 30),
                      c(186, 100),
                      c(51, 20),
                      c(87,40))

block_m_each
Z <- block_ra(blocks = hec$Hair, block_m_each = block_m_each)
table(Z, hec$Hair)

declaration <- declare_ra(blocks = hec$Hair, block_m_each = block_m_each)

# show the probability that each unit is assigned to each condition
head(declaration$probabilities_matrix)

# create blocks with randomizr

blocks <- with(hec, paste(Hair, Eye, Sex, sep = "_"))
Z <- block_ra(blocks = blocks)
head(table(blocks, Z))

# create blocks with blockTools

# BlockTools requires that all variables be numeric
numeric_mat <- model.matrix(~Hair+Eye+Sex, data=hec)[,-1]

# BlockTools also requres an id variable
df_forBT <- data.frame(id_var = 1:nrow(numeric_mat), numeric_mat)

# Conducting the actual blocking: let's make trios
out <- block(df_forBT, n.tr = 3, id.vars = "id_var", 
             block.vars = colnames(df_forBT)[-1])

# Extact the block_ids
hec$block_id <- createBlockIDs(out, df_forBT, id.var = "id_var")

# Conduct actual random assignment with randomizr
Z_blocked <- block_ra(blocks = hec$block_id, num_arms = 3)
head(table(hec$block_id, Z_blocked))




