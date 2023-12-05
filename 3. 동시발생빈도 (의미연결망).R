### 동시발생빈도: 의미연결망
#Co-occurrence(동시발생빈도) - 의미연결망분석

# 1. 기본 package 설치
# install.package("widyr")
# install.package("tidyverse")
# install.package("tidytext")

library(widyr)
library(tidyverse)
library(tidytext)

#-----------------------------------------

# 2. 동시출현단어(n 기준)
# 동시출현단어 빈도분석 (같은 문서)

word_pairs <- word_t_tb %>%
  group_by(word) %>%
  filter(n() >= 30) %>%
  ungroup() %>%
  pairwise_count(item = word,
                 feature = no,
                 sort = T)
word_pairs


# 소셜 네트워크에서 중요도를 발견해내는 과정이다.

#-----------------------------------------

# 3. 동시출현단어(cor 기준)
# 동시출현단어 상관계수분석
# 동시출현단어는 가장 많은 빈도수를 의미하는 반면,
# 상관관계(phi)는 두개 단어가 동시에 나오거나, 나오지 않을 확률

word_cors <- word_t_tb %>%
  group_by(word) %>%
  filter(n() >= 30) %>%
  ungroup() %>%
  pairwise_cor(item = word,
               feature = no,
               sort = T)
word_cors

#-----------------------------------------

# 4. 하위단어 분석
word_pairs %>%
  filter(item1 == "chatgpt")
word_pairs

#-----------------------------------------