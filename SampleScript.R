# Luminal A vs Basal

# Read in word clouds
# Get character vector of all background terms
background <- read.csv('~/WordCloudStats/Data/Inflammation_C2_DistUp.csv')
background <- rownames(background)
background <- gsub('[A-Z]+_(.*)','\\1',background)
background <- unlist(strsplit(background,'_'))

# Words we do not want
C2_stopwords <- c("I", "HE", "THEIR", "ARE", "DOING", "YOU'VE", "WE'LL", "WOULDN'T", "WHEN'S", "UNTIL", "BEFORE", "UNDER", "BOTH", "SAME", "ME", "HIM", "THEIRS", "WAS",
                  "WOULD", "WE'VE", "THEY'LL", "SHAN'T", "WHERE'S", "WHILE", "AFTER", "AGAIN", "EACH", "SO", "MY", "HIS", "THEMSELVES", "WERE", "SHOULD", "THEY'VE",
                  "ISN'T", "SHOULDN'T", "WHY'S", "OF", "ABOVE", "FURTHER", "FEW", "THAN", "MYSELF", "HIMSELF", "WHAT", "BE", "COULD", "I'D", "AREN'T", "CAN'T",
                  "HOW'S", "AT", "BELOW", "THEN", "MORE", "TOO", "WE", "SHE", "WHICH", "BEEN", "OUGHT", "YOU'D", "WASN'T", "CANNOT", "A", "BY", "TO", "ONCE",
                  "MOST", "VERY", "OUR", "HER", "WHO", "BEING", "I'M", "HE'D", "WEREN'T", "COULDN'T", "AN", "FOR", "FROM", "HERE", "OTHER", "YOU", "IT", "THAT",
                  "HAD", "SHE'S", "THEY'D", "HADN'T", "THAT'S", "BUT", "AGAINST", "IN", "WHERE", "NO", "YOUR", "ITS", "THESE", "HAVING", "IT'S", "I'LL", "DOESN'T",
                  "WHO'S", "IF", "BETWEEN", "OUT", "WHY", "NOR", "OURSELVES", "HERSELF", "THIS", "HAS", "HE'S", "WE'D", "HAVEN'T", "LET'S", "AND", "ABOUT", "DOWN",
                  "WHEN", "SUCH", "OURS", "HERS", "WHOM", "HAVE", "YOU'RE", "SHE'D", "HASN'T", "MUSTN'T", "THE", "WITH", "UP", "THERE", "SOME", "YOURS", "ITSELF",
                  "THOSE", "DO", "WE'RE", "YOU'LL", "DON'T", "WHAT'S", "OR", "INTO", "ON", "HOW", "NOT", "YOURSELF", "THEY", "AM", "DOES", "THEY'RE", "HE'LL",
                  "DIDN'T", "HERE'S", "BECAUSE", "THROUGH", "OFF", "ALL", "ONLY", "YOURSELVES", "THEM", "IS", "DID", "I'VE", "SHE'LL", "WON'T", "THERE'S", "AS",
                  "DURING", "OVER", "ANY", "OWN", "LISTYOURUNIQUEWORDSHERE", "DN", "UP", "DN1", "UP1", "UP2", "DN2", "HR", "VS", "VIA", "EARLY", "LATE")

# Luminal A
luma <- read.csv('~/WordCloudStats/Data/C2_Inflammation_LumA_DistUp.txt',sep='\n')
luma <- as.character(luma[,1])

# Basal
basal <- read.csv('~/WordCloudStats/Data/C2_Inflammation_Basal_DistUp.txt',sep='\n')
basal <- as.character(basal[,1])

# Run WordCloudAnalysis: Luminal A vs. Background
library(WordCloudAnalysis)
lumavbg <- wordcloudstats(luma,background,c('Luminal A','Background'),avoid.words=C2_stopwords)
luma.sig <- lumavbg$outputs[,'BH Value'] < 0.25

# Run WordCloudAnalysis: Basal vs. Background
basalvbg <- wordcloudstats(basal,background,c('Basal','Background'),avoid.words=C2_stopwords)
basal.sig <- basalvbg$outputs[,'BH Value'] < 0.25

# Generate comparison plot: Luminal A vs. Basal
# Find intersection between significant terms (it's only FOXP3)
# Keep all common terms, regardless of significance to show the high-volume handling of the package
basal.out <- basalvbg$outputs
luma.out <- lumavbg$outputs
overlap <- intersect(rownames(basal.out), rownames(luma.out))
basal.out <- basal.out[overlap,]
luma.out <- luma.out[overlap,]

# Generate some random q-values (betwee 0 and 1 as they would be)
qval <- runif(length(overlap), 0.0, 1.0)
names(qval) <- overlap

# Generate plot
freq <- cbind(lumavbg$frequency[overlap,1], basalvbg$frequency[overlap,1])
count <- cbind(lumavbg$counts[overlap,1], basalvbg$counts[overlap,1])
comparisonplot(freq, count, qval)

 