# Estudando text mining e fazendo os exercícios do livro https://www.tidytextmining.com/

# Antes de instalar o tidytext, rodar:
#install.packages(c("mnormt", "psych", "SnowballC", "hunspell", "broom", "tokenizers", "janeaustenr"))

# script meio em inglês, meio em português. Sorry about that.

#1.2 The unnest_tokens function

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text

# This is a typical character vector that we might want to analyze. 
# In order to turn it into a tidy text dataset, we first need to put it into a data frame.

library(dplyr)
text_df <- data_frame(line = 1:4, text = text)

text_df


# Notice that this data frame containing text isn’t yet compatible with tidy text analysis.
# We can’t filter out words or count which occur most frequently, since each row is made up of multiple combined words. 
# We need to convert this so that it has one-token-per-document-per-row.

# Within our tidy text framework, we need to both break the text into individual tokens 
# (a process called tokenization) and transform it to a tidy data structure. 
# To do this, we use tidytext’s unnest_tokens() function.

library(tidytext)

text_df %>%
  unnest_tokens(word, text)

#obs: sem pontuação e tudo lower case.
#obs2: ainda não está em um jeito que eu possa agrupar em documentos, vejamos...

#####

# 1.3 Tidying the works of Jane Austen

# Let’s use the text of Jane Austen’s 6 completed and published novels  from the janeaustenr package 
# which provides these texts in a one-row-per-line format, where a line is this context is analogous to a literal printed line in a 
# physical book. 

library(janeaustenr)
library(dplyr)
library(stringr)

#olhando a cara do documento:

head(austen_books()) 

# Let’s start with that, and also use mutate() to annotate a linenumber quantity 
# to keep track of lines in the original format and a chapter (using a regex) 
# to find where all the chapters are.

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books
#o ungroup serve para criar a coluna e depois voltar ao formato original

# To work with this as a tidy dataset, we need to restructure it in the one-token-per-row format, 
# which as we saw earlier is done with the unnest_tokens() function.

library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

# This function uses the tokenizers package to separate each line of text in 
# the original data frame into tokens, mas poderia ser n-grams , frases ou outros.

