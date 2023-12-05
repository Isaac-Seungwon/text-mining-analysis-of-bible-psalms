##### 형태소 분석

# 1.1 기본 package 설치
# install.packages("tidyverse")
# install.packages("tidytext")

# 1.2 library 로드
library(KoNLP)
useNIADic() # 단어사전

library(tidyverse)
library(tidytext)



# 2.1 데이터 가져오기
tm_tb <- read_csv('Article content analysis.csv',
                  col_names = TRUE,                       
                  na = ".")
tm_tb



# 2.2 문서 전처리 (정규표현식 이용)
tm_tb <- tm_tb %>%
  mutate(본문 = gsub("[[:cntrl:]]", "", 본문)) %>% # 엔터 등 제거
  mutate(본문 = gsub("[[:punct:]]", " ", 본문)) %>% # 구두점 등 제거
  mutate(본문 = gsub("[[:digit:]]", "", 본문))     # 숫자 제거
tm_tb$본문[2]

# 정규표현식 
# mutate(본문 = gsub("", "", 본문))
# 무엇을 출력할지는 내용을 보면서 생격해 봐야 하나ㅣ
# [:cntrl:]: 제어문자 [\x00-\x1F\x7F]
# [:punct:]: 구두점 [][!"#$%&'()*+,./:;<=>?@\^_`{|}~-]
# [:digit:]: 숫자 [0-9]


# 데이터 전처리



# 3.1 형태소 분석
word_tb <- tm_tb %>%
  select(no, 본문) %>%
  unnest_tokens(input = 본문,
                output = morp,
                token = SimplePos09) %>% # 한글 형태소로 분리
  mutate(word = str_remove(morp, "/.*$")) %>% # 형태소 정보 제거
  filter(str_length(word) >= 2)
word_tb

# token: 어떤 방법으로 token처리를 할 것인가?
# A tibble: 10,585 × 3


# 3.2 명사추출
word_n_tb <- word_tb %>%
  filter(str_detect(morp, "/n"))# 명사 추출
word_n_tb

# A tibble: 7,873 × 3


# 3.3 동사, 형용사 추출
word_p_tb <- word_tb %>%
  filter(str_detect(morp, "/p")) %>% # 동사, 형용사 추출
  mutate(word = str_replace(morp, "/.*$", "다")) # 형태소 정보 제거
word_p_tb

# A tibble: 817 × 3



# 통합
word_t_tb <- bind_rows(word_n_tb, word_p_tb)
word_t_tb



# 4. 단어 정리

# 단어 바꾸기
word_t_tb <- word_t_tb %>%
  mutate(word = gsub("ai", "인공지능", word)) %>% 
  mutate(word = gsub("바드는", "바드", word)) %>% 
  mutate(word = gsub("챗지피티", "chatgpt", word)) %>%
  mutate(word = gsub("지피티", "chatgpt", word)) %>%  
  mutate(word = gsub("챗gpt를", "chatgpt", word)) %>%
  mutate(word = gsub("챗gpt", "chatgpt", word))
word_t_tb
# 같은 단어를 한단어로 변경

# 단어 삭제
st_word <- tibble(word=c("대하다", "통하다", "따르다", "위하다", "지나다", "에서", "보이다", "때문", "아니다"))

word_t_tb <- word_t_tb %>%
  anti_join(st_word, by="word")

word_t_tb


#-----------------------------------------

##### 단어 빈도 분석

# 1.1 기본 package 설치
# install.packages("")
# install.packages("tidyverse")
# install.packages("tidytext")

# 1.2 library 로드
library(KoNLP)
useNIADic() # 단어사전

library(tidyverse)
library(tidytext)


# 1. 단어 빈도 및 퍼센테이지(%) 계산
word_count <- word_t_tb %>%
  count(word, sort = TRUE) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()
word_count


# 2. 단어 확인
word_count %>%
  slice_max(n, n = 100) %>%
  print(n = 100)

# 전체 데이터에서 상위 60개만 확인
word_count %>%
  slice_max(n, n = 60) %>%
  print(n = 60)

# 전체 데이터에서 상위 60개만 확인
# 데이터 양을 보고 slice_max를 이용해서 보면 된다.
# filter기능을 이용하면 데이터가 몇 개 이상 모여있는지 확인할 수 있다.

# 변경: 다양한
# 삭제: 에서, 때문, 아니다
# 잘 모르겠으면 찾아본다.


# 3. 단어빈도 그래프
word_count %>%
  mutate(word = reorder(word, n)) %>%
  slice_max(n, n = 30) %>%
  ggplot(mapping = aes(x = n, #n을 쓸지 prop을 쓸지 설정
                       y = word)) +
  geom_col()

# reorder의 사용 유무에 따라 나중에 보여지는 데이터 결과가 달라지는 경우가 있다.
# 데이터가 많아도 100개 이내로 확인한다. 가끔 보면 1000개 이상으로 논문을 작성하는 사람도 있는데 그런 것은 별로 좋지 않다.




#-----------------------------------------


#### 워드클라우드

# 1. package 설치
# install.packages("ggwordcloud")
library(ggwordcloud)

set.seed(123)
color <- sample.int(n = 10,
                    size = nrow(word_count %>%
                                  slice_max(n, n = 50)),
                    replace = TRUE)



# 검은색으로 그리기
set.seed(123)
word_count %>%
  filter(n > 20) %>%
  ggplot(mapping = aes(label = word,
                       size = n)) + 
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_minimal()



# 여러색으로 그리기
set.seed(123)
color <- sample.int(n = 10,  # 색 개수 지정 
                    size = nrow(word_count %>%
                                  filter(n > 20)), # 단어에 색 넣기. 
                    replace = TRUE)

word_count %>%
  filter(n > 20) %>%        # 위 color의 size의 filter n>20과 같아야 함
  ggplot(mapping = aes(label = word,
                       size = n,
                       color = factor(color))) + 
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_minimal()




# 2. 단색으로 그리기

word_count %>%
  slice_max(n,n = 50) %>%
  ggplot(mapping = aes(label = word,
                       size = n,
                       color = factor(color))) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkred", high = "red") +
  theme_minimal()

# slicemx는 매칭이 되어야 한다.
# 필터로 처리하면 똑같이 필터로 처리, 50개면 50개로 처리한다.