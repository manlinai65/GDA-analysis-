# Cluster Analysis /R Assignment 7
## POSM / UNIL / Marina Obata

# Load packages -----------------------------------------------------------

library(tidyverse)
library(cluster)
library(FactoMineR)
library(factoextra)

# Load data ---------------------------------------------------------------

df_cluster <- read_rds("Data/data-cluster.rds")

# (Very) quick EDA --------------------------------------------------------

df_cluster
df_cluster %>% summary()
df_cluster %>% colnames()

# Prepare subset for Cluster Analysis -------------------------------------

# Choose some variables
df_cluster_var1 <-
    df_cluster %>%
    select(
        gender,
        absent,
        student_preschool,
        books,
        parents_high_edu,
        parents_high_occ,
        belonging_scale,
        bullying_scale,
        like_maths_scale,
        engaging_teaching_m_scale,
        student_conf_m_scale,
        early_literacy_scale,
        home_resources_scale,
        early_tasks_scale,
        early_numbers_scale,
        school_perception_scale
    )

##M This time, I tried to include more variables which is also showing the
# demographic and factual information, since it might help to characterize the
# each group of cluster.

# Taking a sample
df_cluster_sample1 <-
    df_cluster_var1 %>%
    sample_n(., 200)

df_cluster_sample1

# Welcome to Cluster Analysis ---------------------------------------------

# First things first, let's create a distance matrix.
distances1 <- dist(df_cluster_sample1)

# Organizing NAs
distance1_omit <- distances1 %>% na.omit()

# Let's visualise the results (how many rows and columns do you guess?)
fviz_dist(distance1_omit)

# We will now apply some exciting algorithms.

# Hierarchical Cluster Analysis -------------------------------------------

# First, the first type of cluster approach that does not require a given
# number of clusters, Hierarchical Clustering.

cluster_hierarchical_01 <- hclust(distance1_omit)

# Let us take a look in detail
summary(cluster_hierarchical_01)
str(cluster_hierarchical_01)

# Enter: Base R plotting
plot(cluster_hierarchical_01)

##M It seems that there are three large groups in level of height 20.
# At the highest level, there are only two clusters existing, however, the third
# cluster is not that small to be dismissed.

# What can R tell us about Hierarchical Clustering
?hclust

# Apply another method right now:
cluster_hierarchical_02 <- hclust(distance1_omit, method = "ward.D")
cluster_hierarchical_02 <- NULL

# Enter: ggplot2
fviz_dend(cluster_hierarchical_01,
          k = 3,
          cex = 0.5,
          rect = TRUE)

##M According to the cluster dendrogram with kmean=3, it showed the number of
# clusters as 3 at the level of height 20.

# A closer look at how the algorithm works
# What happens during the individual steps:
cluster_hierarchical_01$merge
# What about the height (= similarity):
cluster_hierarchical_01$height

# It is also informative to look closely at individual cases
df_cluster_sample1 %>%
    rownames_to_column() %>%
    slice(41, 98, 104)

##M I had a look on three individuals which belongs to different groups of 3 clustres.
# It seems there is not much relation between parents's education and occupation level
# and group of clusters, the variables which showed difference across groups are,
# length of preschool and beloning scale, school perception scale, early literacy, number scale
# and home resources scale. However, cannot state anything clear from just three samples.



# Partitioning based Clustering -------------------------------------------

# Partitioning methods, such as k-means clustering require the users to specify
# the number of clusters to be generated.
# Note: we switch back to the full data set!

head(df_cluster_var1)
# Drop the variables which is not numeric
df_cluster_var1_scaled <- scale(df_cluster_var1[-c(0,1:6)])

nbclust1 <- fviz_nbclust(
    df_cluster_var1_scaled,
    FUNcluster = kmeans,
    method = c('silhouette', 'wss', 'gap_stat'),
    diss = NULL,
    k.max = 10,
    nboot = 100,
    verbose = interactive
    )

?fviz_nbclust

##M The plot showed the optimal number of cluster as 2. However, given the previous results,
# and the fact that the plot is showing 3 cluster is still upper the "bended" point, I assume
# 3 clusters is still acceptable, however, if it is less than 2 or more than 5 clusters, it is going
# to be not practical.


# Bad cluster -------------------------------------------------------------
cluster_kmeans1 <- kmeans(df_cluster_var1_scaled, 5)

# Let us take a look in detail
summary(cluster_kmeans1)
str(cluster_kmeans1)

# Thanks to factoextra we can directly access visualisation:
?fviz_cluster
# Why do we have to prive the raw data frame also?
fviz_cluster(cluster_kmeans1,df_cluster_var1_scaled,labelsize = 1)
# Have you recognised the Dimensions? Where do they come from?
fviz_cluster(cluster_kmeans1, df_cluster_var1_scaled, axes = c(2, 3), labelsize = 1)
fviz_cluster(cluster_kmeans1, df_cluster_var1_scaled, axes = c(1, 3), labelsize = 1)

##M As I assumed, the clustering is not good because cluster No.5 is totally overlapped by
# other clusters for dimension 1 and 2. Even for other dimensions, there is no
# clear independent cluster can be observed.

# What exactly does kmeans do?

?kmeans

cluster_kmeans1$totss
cluster_kmeans1$tot.withinss
cluster_kmeans1$betweenss
cluster_kmeans1$withinss
cluster_kmeans1$cluster
cluster_kmeans1$size
cluster_kmeans1$iter

# Let's try another approach

?pam

cluster_pam1 <- pam(df_cluster_var1_scaled, k = 5)

# An elegant way to visualise the algorithm
?fviz_silhouette
fviz_silhouette(cluster_pam1)

# Here is the actual classifications …
cluster_pam1$clustering

# … which we can now add to the original data as a new variable:
df_clustered1 <-
    df_cluster %>%
    add_column(
        cluster = cluster_pam1$clustering
    )

df_clustered1 %>% View()


# Interpret Cluster Analysis results --------------------------------------

# We can now use this information to better understand the clusters.
# You can also apply the follow methods on Hierarchical Cluster Analysis results.
# Ths is a perfect situation for a new "private" function just for this!
quick_analyse_clusters1 <- function(name_of_the_variable) {
    df_clustered1 %>%
        group_by(cluster) %>%
        # It's a little nasty here. We need to "tunnel" the data with
        # the so called curly-curly approach.
        # If you're interested, take a look at the rlang package.
        count( {{ name_of_the_variable }} ) %>%
        mutate(freq = n / sum(n))
}

# Now we can do a clean and fast analysis of something like
# "supplementary variables" or better cluster-variable relations.
quick_analyse_clusters1(gender)
##M 1 more boys, 2,3,4,5 more girls
quick_analyse_clusters1(books)
##M 1,2 students having more books, 4 neutral, 3,5 less books
quick_analyse_clusters1(parents_high_edu)
##M 2 higher education, 1,3, middle, 4,5 lower education
quick_analyse_clusters1(parents_high_occ)
##M 2 higher occupation, 2,3,4 more clarical 5 more skilled
quick_analyse_clusters1(student_preschool)
##M 2 longer, 3,4,5 more students did not attend, 1 neutral
quick_analyse_clusters1(father_native)
##M 4,5 less native father
quick_analyse_clusters1(absent)
##M 2 higher in never absent, 1,4,5 more often absent,3 neutral
quick_analyse_clusters1(breakfast)
##M 1,2 almost everyday 3,4  neutral 5 less often
quick_analyse_clusters1(test_language)
##M 2,5 more natives, 1,3 neutral 4 less frequent
quick_analyse_clusters1(born_in_country)
##M High 3,2,1,4,5 Low

##M As for the clustering of supplemental variables, there are sometimes more than 2 clusters showing the
# neutral position, which is not appropliate to grasp the characteristic of each cluster.
# However, the result showed that cluster 2 has more elite parents and having good cares,
# 5 has more non-native origin and having less care from parents. As for 1,3,4 it is not clear.

# But why do we have to do this first? (Subset the non numeric variables)
df_cluster_var1_subset <- subset(df_cluster_var1, select = -c(0,1:6) )

df_cluster_var_plot1 <-
    df_cluster_var1_subset%>%
    pivot_longer(cols = 1:10)

# Only after this data transformation can we plot:
ggplot(df_cluster_var_plot1) +
    geom_boxplot(aes(name, value)) +
    ggtitle("Cluster variables")

# Let's look at the individual variables by cluster.
# And why not simply use a custom function for this?

quick_analyse_clusters_boxplot1 <- function(name_of_the_variable) {
    ggplot(df_clustered1) +
        geom_boxplot(
            aes(
                x = cluster,
                # curly-curly ;-)
                y = {{ name_of_the_variable }},
                group = cluster
            )
        ) +
        # Why are we changing the x-scale?
        scale_x_continuous(breaks = df_clustered1$cluster %>% unique()) +
        ggtitle("Variable by clusters")
}

quick_analyse_clusters_boxplot1(belonging_scale)
##M High 2, 5, 1, (3,4)  low
quick_analyse_clusters_boxplot1(bullying_scale)
##M High 2, 5, (3,4), 1  low
quick_analyse_clusters_boxplot1(like_maths_scale)
##M High 2, 1, 5, (3,4)  low
quick_analyse_clusters_boxplot1(engaging_teaching_m_scale)
##M High 2, 5, 1, (3,4)  low
quick_analyse_clusters_boxplot1(student_conf_m_scale)
##M High 2, 1, 5, (3,4)  low
quick_analyse_clusters_boxplot1(home_resources_scale)
##M High (1,2),(3,4),5  low
quick_analyse_clusters_boxplot1(early_literacy_scale)
##M High 4, 2, 1, (3,5)  low
quick_analyse_clusters_boxplot1(early_numbers_scale)
##M High 4, 2, 1, (3,5)  low
quick_analyse_clusters_boxplot1(early_tasks_scale)
##M High (2, 4), 1, 3, 5  low
quick_analyse_clusters_boxplot1(school_perception_scale)
##M High 2, 5, 1, (3,4)  low

##M As a result of closer look on varialbles which are showing scales, cluster No.3 and 4 had
# very similar values across the variables, which is showing the clustreing is not appropriate.
# Otherwise, from the number of 5 clusters, cluster No.2 showed the characteristic of higher confidence
# and motivation for studing and higher score for early preparations.
# Cluster No. 5 showed the higer moitvarion for learning and school belonging, however showed lower scores
# towards early preparation. If I name each cluster,
# cluster 1 : Rich home resource and not bullied, but standard early preparation and learning motivation (rich family)
# cluster 2 : Higher learning motivation and school belonging and higher early preparation (elite type)
# cluster 3 : Lower motivation for learning and lower school belonging and lower early preparation (non-learning motivation)
# cluster 4 : Lower motivation for learning and lower school belonging but well prepared for literacy and numbers (non-leaning motivation but having preparation)
# cluster 5 : Poor home resources and preparation, middle motivation for learning (mediocre type)

# Good cluster -------------------------------------------------------------
cluster_kmeans2 <- kmeans(df_cluster_var1_scaled, 3)

# Let us take a look in detail
summary(cluster_kmeans2)
str(cluster_kmeans2)

# Why do we have to prive the raw data frame also?
fviz_cluster(cluster_kmeans2,df_cluster_var1_scaled,labelsize = 1)
# Have you recognised the Dimensions? Where do they come from?
fviz_cluster(cluster_kmeans2, df_cluster_var1_scaled, axes = c(2, 3), labelsize = 1)
fviz_cluster(cluster_kmeans2, df_cluster_var1_scaled, axes = c(1, 3), labelsize = 1)

##M Even though, the "fviz_nbclust" showed the optimized number of cluster as 2,
#  3 clusters can be considered as better since dimension 1 and 2 shows the clear
# independecy of three cluster groups, it might keep the specific charactetistics for
# cluster No.3.

# What exactly does kmeans do?
cluster_kmeans2$totss
cluster_kmeans2$tot.withinss
cluster_kmeans2$betweenss
cluster_kmeans2$withinss
cluster_kmeans2$cluster
cluster_kmeans2$size
cluster_kmeans2$iter

##M The value of between-cluster sum of square got smaller compared to 5 clusters.

# Let's try another approach
cluster_pam2 <- pam(df_cluster_var1_scaled, k = 3)

# An elegant way to visualise the algorithm
fviz_silhouette(cluster_pam2)

# Here is the actual classifications …
cluster_pam2$clustering

# … which we can now add to the original data as a new variable:
df_clustered2 <-
    df_cluster %>%
    add_column(
        cluster = cluster_pam2$clustering
    )

df_clustered2 %>% View()


# Interpret Cluster Analysis results --------------------------------------

# We can now use this information to better understand the clusters.
# You can also apply the follow methods on Hierarchical Cluster Analysis results.

# Ths is a perfect situation for a new "private" function just for this!
quick_analyse_clusters2 <- function(name_of_the_variable) {
    df_clustered2 %>%
        group_by(cluster) %>%
        # It's a little nasty here. We need to "tunnel" the data with
        # the so called curly-curly approach.
        # If you're interested, take a look at the rlang package.
        count( {{ name_of_the_variable }} ) %>%
        mutate(freq = n / sum(n))
}

# Now we can do a clean and fast analysis of something like
# "supplementary variables" or better cluster-variable relations.
quick_analyse_clusters2(gender)
##M 1 more boys, 2,3 more girls
quick_analyse_clusters2(books)
##M 3 having more books, 2 middle, 1 having less books
quick_analyse_clusters2(parents_high_edu)
##M Not obvious differences between clusters
quick_analyse_clusters2(parents_high_occ)
##M 2,3 more professional, 1 more clarical
quick_analyse_clusters2(student_preschool)
##M 1: More students who did not attend
#   2,3 : More students who attended
quick_analyse_clusters2(father_native)
##M 1,2: higher father native than 3
quick_analyse_clusters2(absent)
##M 2 less absent, 3 middle, 1 more absent
quick_analyse_clusters2(breakfast)
##M 2: more stundets eat everyday and less students skip than 1,3
quick_analyse_clusters2(test_language)
##M Not obvious differences between clusters
quick_analyse_clusters2(born_in_country)
##M 1: more natives than 2,3

##M Showing simpler clustreing for supplementary variables compared to 5 clusters.
# Cluter 1: Natives, with less preparation for preschool, having parents working clearical job
# Cluter 2: Father natives, having good care from parents
# Cluter 3: More foreign borns, having more books and more preparation

# Let's look at the individual variables by cluster.
# And why not simply use a custom function for this?

quick_analyse_clusters_boxplot2 <- function(name_of_the_variable) {
    ggplot(df_clustered2) +
        geom_boxplot(
            aes(
                x = cluster,
                # curly-curly ;-)
                y = {{ name_of_the_variable }},
                group = cluster
            )
        ) +
        # Why are we changing the x-scale?
        scale_x_continuous(breaks = df_clustered2$cluster %>% unique()) +
        ggtitle("Variable by clusters")
}

names(df_cluster)
quick_analyse_clusters_boxplot2(belonging_scale)
##M High  2,1,3 Low
quick_analyse_clusters_boxplot2(bullying_scale)
##M High  2,1,3 Low
quick_analyse_clusters_boxplot2(like_maths_scale)
##M High  2,1,3 Low
quick_analyse_clusters_boxplot2(engaging_teaching_m_scale)
##M High  2,1,3 Low
quick_analyse_clusters_boxplot2(student_conf_m_scale)
##M High  2,1,3 Low
quick_analyse_clusters_boxplot2(home_resources_scale)
##M tie , 1 more outliers
quick_analyse_clusters_boxplot2(early_literacy_scale)
##M High  3,2,1 Low
quick_analyse_clusters_boxplot2(early_numbers_scale)
##M High  3,2,1 Low
quick_analyse_clusters_boxplot2(early_tasks_scale)
##M High 3,2,1 Low
quick_analyse_clusters_boxplot2(school_perception_scale)
##M High  2,1,3 Low

##M It is more obvious to see the charactetistics of each cluster than 5 cluster groups,
# also, could see the consistency towards grouping.
# To interpret, cluter 2 has higher belonging scale and motivation towards learning,
# cluster 3 has higher score for early preparation, however, they are not motivated for
# school. Cluster 1 has middle motivation towards school with lowest score for early preparation.
# Home resources got no more difference between clusters.

#Cluster 1:Mediocre type towards school with less early prepration
#Cluster 2:Highly motivated towards school with ordinallty early preparation
#Cluster 3:Well prepared to school but not motivated for school

# It is also corresponding well with the interpretation of clustering of supplemental variables.

## Thus, we could see from this sample that early preparation is not having strong relations
# between motivation for school after getting in. It can be parents' good care to have higer learning motivations.






