# === 감성분석 === #

library(tidyverse)
library(tidytext)


# 감성사전 불러오기
senti_dic_tb <- read_delim("Sentiword_Dict.txt",
                           delim = "\t",
                           col_names = c("word", "score"))
senti_dic_tb %>%
  count(score)

# 감성사전 단어 14,845개
# 완전 부정(-2) 4799개
# 부정(-1) 5031개
# 중립(0) 154개
# 긍정(1) 2268개
# 완전 긍정(2) 2603개

senti_dic_tb %>%
  mutate(감정점수 = case_when(score >= 1 ~ "긍정",
                          score <= -1 ~ "부정",
                          TRUE ~ "중립"))%>% # 긍정도 부정도 아닐 경우 중립
  count(감정점수)

# 긍정 4871개
# 부정 9830개
# 중립 154개

# case_when: 조건절로서 이용되며, 다중if 구문과 혼합할 수 있다.


#형태소로 처리한 뒤 분석하는 방법과 그렇지 않은 경우가 있다.
# '~는'로 분석할 지, '~다'로 분석할 지 선택할 수 있다.


# 감성분석
# 형태소 분석 없는 상태
senti_word_counts <- tm_tb %>%
  # filter(!(page == 1)) %>%
  select(page, verse) %>%
  unnest_tokens(input = verse,
                output = word) %>%
  inner_join(senti_dic_tb) %>%
  group_by(word) %>%
  summarise(score = sum(score)) %>%
  ungroup() %>%
  mutate(n = abs(score)) %>%
  mutate(감정점수 = case_when(score >= 1 ~ "긍정",
                          score <= -1 ~ "부정",
                          TRUE ~ "중립"))

# 긍정 단어부터 출력
senti_word_counts %>%
  arrange(desc(score))

  
# 형태소 분석 이후 상태
senti_word_counts <- tm_tb %>% 
  select(page, verse) %>%
  unnest_tokens(input = verse,
                output = morp,
                token = SimplePos09) %>%        # 한글 형태소로 분리
  mutate(word = str_remove(morp, "/.*$")) %>% # 형태소 정보 제거
  filter(str_length(word)>=2) %>%
  inner_join(senti_dic_tb) %>%        # 감성사전과 매핑
  group_by(word) %>%
  summarise(score = sum(score)) %>%
  ungroup() %>%
  mutate(n = abs(score)) %>%
  mutate(sentiment = case_when(score >= 1 ~ "긍정",
                               score <= -1 ~ "부정", 
                               TRUE  ~ "중립"))

# 긍정 단어부터 출력
senti_word_counts %>%
  arrange(desc(score))

senti_word_counts %>%
  arrange(desc(n)) %>%
  print(n = 30)


# 시각화
# word count(n)
senti_word_counts %>%
  filter(!(sentiment == "중립")) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")

# word count(score)
senti_word_counts %>%
  filter(!(sentiment == "중립")) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, score)) %>%
  ggplot(aes(x = score, 
             y = word, 
             fill = score > 0)) +
  geom_col(show.legend = FALSE)



# wordcolud
set.seed(123)
senti_word_counts %>%
  filter(!(sentiment == "중립")) %>%
  group_by(sentiment) %>%
  # slice_max(n, n = 10) %>%
  ungroup() %>%
  ggplot(mapping = aes(label = word,
                       size = n,
                       color = sentiment,
                       angle_group = score < 0)) + 
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_minimal()


# wordcolud(구분)
set.seed(123)
senti_word_counts %>%
  filter(!(sentiment == "중립")) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 20) %>%
  ggplot(mapping = aes(label = word,
                       size = n,
                       color = sentiment)) + 
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  facet_wrap(~ sentiment)
