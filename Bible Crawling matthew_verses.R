# Bible Crawling Project

# === library 설정 === #

useNIADic() # 단어사전
library(KoNLP)
library(multilinguer)
library(tidytext)
library(tidyverse)

# 동시발생빈도
library(widyr)
library(tidygraph)
library(ggraph)

library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(wordcloud2)
library(ggwordcloud)
library(stm)
library(furrr)
library(ggthemes)

# === 성경 크롤링 === #

# 크롤링할 URL 주소
base_url <- "https://www.bskorea.or.kr/bible/korbibReadpage.php?version=GAE&book=mat&chap="

# 크롤링할 페이지 범위
start_page <- 1
end_page <- 28

# 결과를 저장할 데이터프레임
df <- data.frame(page = integer(),
                 verse = character(),
                 stringsAsFactors = FALSE)

# 페이지별로 크롤링 수행
for (page in start_page:end_page) {
  # 페이지 URL 생성
  url <- paste0(base_url, page)
  
  # 페이지 내용 가져오기
  page_content <- read_html(url)
  
  # 성경 구절 추출
  verse <- page_content %>%
    html_nodes("#tdBible1 > span") %>%
    html_text() %>%
    str_trim()
  
  # 추출한 구절 데이터를 임시 데이터프레임에 추가
  temp_df <- data.frame(page = page, verse = verse, stringsAsFactors = FALSE)
  
  # 임시 데이터프레임을 결과 데이터프레임에 병합
  df <- rbind(df, temp_df)
}

# 결과를 CSV 파일로 출력
output_file <- "matthew_verses.csv"
write_excel_csv(df, path = output_file)

# 크롤링 결과 출력
unique_pages <- unique(df$page)
for (page in unique_pages) {
  cat(paste0("=== Page ", page, " ===\n"))
  cat(df[df$page == page, "verse"], "\n\n")
}

# cat() 함수: 출력 결과를 콘솔에 텍스트 형식으로 표시하는 데 사용되는 R의 내장 함수
# 위의 코드에서 cat() 함수는 크롤링 결과를 페이지별로 출력하는 역할을 한다.

# paste0() 함수: "=== Page X ===" 형식의 페이지 헤더를 생성한 후, 이를 cat() 함수를 통해 콘솔에 출력한다. 그 다음, df[df$page == page, "verse"]를 사용하여 해당 페이지에 대한 구절 데이터를 선택하고, 다시 cat() 함수를 사용하여 구절 데이터를 콘솔에 출력한다.



# === 형태소 분석 === #

# 데이터 가져오기
tm_tb <- read_csv('matthew_verses.csv',
                  col_names = TRUE,                       
                  na = ".")
tm_tb

# 문서 전처리 (정규표현식 이용)
tm_tb <- tm_tb %>%
  mutate(verse = gsub("[[:cntrl:]]", "", verse)) %>% # 엔터 등 제거
  mutate(verse = gsub("[[:punct:]]", "", verse)) %>% # 구두점 등 제거
  mutate(verse = gsub("[[:digit:]]", "", verse))     # 숫자 제거
tm_tb$verse

# 형태소 분석
word_tb <- tm_tb %>%
  select(page, verse) %>%
  unnest_tokens(input = verse,
                output = morp,
                token = SimplePos09) %>% # 한글 형태소로 분리
  mutate(word = str_remove(morp, "/.*$")) %>% # 형태소 정보 제거
  filter(str_length(word) >= 2)
word_tb

# 명사추출
word_n_tb <- word_tb %>%
  filter(str_detect(morp, "/n"))# 명사 추출
word_n_tb

# 동사, 형용사 추출
word_p_tb <- word_tb %>%
  filter(str_detect(morp, "/p")) %>% # 동사, 형용사 추출
  mutate(word = str_replace(morp, "/.*$", "다")) # 형태소 정보 제거
word_p_tb

# 통합
word_t_tb <- bind_rows(word_n_tb, word_p_tb)
word_t_tb

### 단어 정리
# 단어 바꾸기
word_t_tb <- word_t_tb %>%
  mutate(word = gsub("제자들이", "제자", word)) %>%
  mutate(word = gsub("제자들", "제자", word)) %>%
  mutate(word = gsub("대제사장들이", "대제사장", word)) %>%
  mutate(word = gsub("대제사장들", "대제사장", word)) %>%
  mutate(word = gsub("서기관들이", "서기관", word)) %>%
  mutate(word = gsub("서기관들", "서기관", word)) %>%
  mutate(word = gsub("바리새인들이", "바리새인", word)) %>%
  mutate(word = gsub("바리새인들", "바리새인", word)) %>%
  mutate(word = gsub("들이", "", word))
word_t_tb

# 단어 삭제
st_word <- tibble(word=c("^ㄱ", "^ㄴ"))

# 불용어 제거
word_t_tb <- word_t_tb %>%
  anti_join(st_word, by="word")
word_t_tb

### 단어 빈도 분석
# 단어 빈도 및 퍼센테이지(%) 계산
word_count <- word_t_tb %>%
  count(word, sort = TRUE) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()
word_count

# 단어 확인 (상위 100개 확인)
word_count %>%
  slice_max(n, n = 100) %>%
  print(n = 100)

# 단어 확인 (상위 60개 확인)
word_count %>%
  slice_max(n, n = 60) %>%
  print(n = 60)

### 단어빈도 그래프
word_count %>%
  mutate(word = reorder(word, n)) %>%
  slice_max(n, n = 30) %>%
  ggplot(mapping = aes(x = n, #n을 쓸지 prop을 쓸지 설정
                       y = word)) +
  geom_col()



# === 워드클라우드 === #

# 검은색 워드클라우드
set.seed(123)
word_count %>%
  filter(n > 20) %>%
  ggplot(mapping = aes(label = word,
                       size = n)) + 
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_minimal()

# 단색 워드클라우드
set.seed(123)
word_count %>%
  slice_max(prop, n = 20) %>%
  ggplot(mapping = aes(label = word,
                       size = prop,
                       color = prop)) + 
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkred", high = "red")

# 무작위 색상 생성
set.seed(123)
color <- sample.int(n = 50,
                    size = nrow(word_count %>%
                                  slice_max(n, n = 50)),
                    replace = TRUE)

# word_count에서 가장 많이 등장하는 단어 상위 50개를 무작위로 선택하고, 그 개수에 맞게 임의의 색상을 생성한다. 이 작업은 후속 시각화 작업이나 데이터 표현을 위해 각 단어에 임의의 색상을 부여하기 위한 것이다.

# 여러 색 워드클라우드
set.seed(123)
color <- sample.int(n = 20,  # 색 갯수 지정 
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



# === Co-occurrence(동시발생빈도) 의미연결망분석 === #

# 동시출현단어(n 기준) 빈도분석
word_pairs <- word_t_tb %>%
  group_by(word) %>%
  filter(n() >= 30) %>%
  ungroup() %>%
  pairwise_count(item = word,
                 feature = page,
                 sort = T)
word_pairs

# word_pairs 데이터프레임
# word_t_tb 데이터프레임에서 등장 빈도가 30 이상인 단어들을 선택한다.
# pairwise_count() 함수를 사용하여 단어 간의 등장 관계를 계산한다.
# 이때, item 매개변수에는 단어를, feature 매개변수에는 페이지 번호를 지정한다.
# 결과는 item1, item2, n 열로 구성된 표로, 각 단어 쌍의 등장 횟수를 나타낸다.
# 이는 단어 간의 연관성을 파악하기 위해 사용된다.


# 동시출현단어(cor 기준) 상관계수분석
word_cors <- word_t_tb %>%
  group_by(word) %>%
  filter(n() >= 30) %>%
  ungroup() %>%
  pairwise_cor(item = word,
               feature = page,
               sort = T)
word_cors

# word_cors 데이터프레임
# word_t_tb 데이터프레임에서 등장 빈도가 30 이상인 단어들을 선택한다.
# pairwise_cor() 함수를 사용하여 단어 간의 상관관계를 계산한다.
# 이때, item 매개변수에는 단어를, feature 매개변수에는 페이지 번호를 지정한다.
# 결과는 item1, item2, correlation 열로 구성된 표로, 각 단어 쌍의 상관관계를 나타낸다.
# 이는 단어 간의 의미적 연결성을 파악하기 위해 사용된다.

# word_pairs는 두 단어가 함께 등장한 횟수를 계산하여 단어 간의 연관성을 확인하고, word_cors는 단어 간의 상관관계를 계산하여 의미적 연결성을 파악한다.
# 단어 간의 연관성은 통계적인 관계를 중심으로 분석되며, 두 단어가 함께 등장하는 경향을 나타내는 반면, 의미적 연결성은 단어들의 의미나 주제와 관련하여 분석되며, 단어들 간의 의미적 유사성을 파악한다.



# === 동시발생빈도 시각화 === #

# centrality 확인
# ppt 설명(Degree centrality(연결 중심성)

set.seed(123)
word_pairs_graph <- word_pairs %>%
  filter(n >= 10) %>%
  as_tbl_graph(directed = F) %>%
  mutate(cent_dgr = centrality_degree(),
         cent_btw = centrality_betweenness(),
         cent_cls = centrality_closeness(),
         cent_egn = centrality_eigen(),
         cent_pg = centrality_pagerank(),
         group = as.factor(group_infomap()))

word_pairs_graph

# table형식으로 엑셀에 저장
as_tibble(word_pairs_graph) %>%
  arrange(desc(cent_dgr)) %>%
  head(n=100) %>%
  write_excel_csv("matthew_verses_word_pairs_graph.xls")


# 동시발생빈도 시각화
# 기본테이블 그래프
word_pairs %>%
  slice_max(n, n = 100) %>%
  as_tbl_graph(directed = F) %>%
  ggraph(layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(mapping = aes(label = name),
                 vjust = 1,
                 hjust = 1)

# ggraph(layout = "fr"): 회색 기본 레이아웃 생성
# geom_edge_link(): 노드와 노드간의 연결선을 화면에 생성
# geom_node_point(): 노드의 점을 화면에 생성
# geom_node_text(aes(label = name): 노드의 이름을 화면에 생성
# vjust = 1, hjust = 1: 텍스트를 점의 x축, y축에서 거리를 두게 함


# 연결중심성 그래프
word_pairs_graph %>%
  as_tbl_graph(directed = F) %>%
  ggraph(layout = "fr") +
  geom_edge_link() +
  geom_node_point(mapping = aes(size = cent_dgr)) +
  geom_node_text(mapping = aes(label = name),
                 vjust = 1,
                 hjust = 1)


# 연결중심 그래프 (색)
word_pairs %>%
  slice_max(n, n = 100) %>%
  as_tbl_graph(directed = F) %>%
  activate(nodes) %>%
  mutate(centrality = centrality_degree()) %>%
  activate(edges) %>%
  mutate(betweenness = centrality_edge_betweenness()) %>%
  ggraph(layout = 'kk') + # 여기서부터는 ggraph의 파트이므로 +로 연결
  geom_edge_link(aes(alpha = betweenness)) + # 색깔을 betweenness로 설정
  geom_node_point(aes(size = centrality,
                      colour = centrality)) +
  scale_color_continuous(guide = 'legend') +
  geom_node_text(aes(label = name),
                 vjust = 1,
                 hjust = 1) +
  theme_graph()

# betweenness: 중간, 사이
# centrality: 중심


# pagelink 그래프
word_pairs %>%
  slice_max(n, n = 100) %>%
  as_tbl_graph(directed = F) %>%
  activate(nodes) %>%
  mutate(similarity = node_similarity_with(which.max(centrality_pagerank()))) %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link(colour = "pink") + 
  geom_node_point(aes(size = similarity), colour = 'steelblue') + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_graph()


# 여러 색 그래프
arrow <- grid::arrow(type = "closed", 
                     length = unit(.05, "inches"))

word_pairs_graph %>%
  ggraph(layout = "fr") +
  geom_edge_link(edge_color = "darkred",
                 arrow = arrow,
                 show.legend = FALSE) +
  geom_node_point(aes(size = cent_dgr,   # 노드 크기
                      color = group),
                  show.legend = FALSE) +
  geom_node_text(aes(label = name), 
                 repel = TRUE,          # repel =TRUE: 텍스트가 겹치지 않게
                 point.padding = unit(0.2, "lines")) +    
  theme_void() # 격자 없애기


# 하위단어 분석
word_pairs %>%
  filter(item1 == "이르다")

word_pairs %>%
  filter(item1 == "예수")

word_pairs %>%
  filter(item1 == "하나님")

word_pairs %>%
  filter(item1 %in% c("하나님", "예수")) %>%
  group_by(item1) %>%
  slice_max(n, n = 20) %>%
  ungroup() %>%
  mutate(item2 = reorder_within(item2, n, item1)) %>%
  ggplot(aes(x = n,
             y = item2,
             fill = item1)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ item1,
             ncol = 2,
             scales = "free") +
  scale_y_reordered()

# 학습과 가장 많이 나타나는 단어와 상관관계가 높은 단어
# show.legend = FALSE: 범례 삭제
# scales = "free"): sclaes를 free로 하므로 별개로 나온다
# 상위 50위 이내에서 보여주는 것이 좋다. (단어 개수는 50~100개)

word_pairs %>%
  filter(item1 %in% c("이르다", "사람")) %>%
  group_by(item1) %>%
  slice_max(n, n = 20) %>%
  ungroup() %>%
  mutate(item2 = reorder_within(item2, n, item1)) %>%
  ggplot(aes(x = n,
             y = item2,
             fill = item1)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ item1,
             ncol = 2,
             scales = "free") +
  scale_y_reordered()



# === 토픽 분석 === #

# 문서-단어(dtm) 행렬 만들기


# tf-idf 만들기
word_t_idf <- word_t_tb %>%
  count(page, word, sort = T) %>%
  bind_tf_idf(word, page, n)

# dfm 만들기
word_dfm <- word_t_idf %>%
  cast_dfm(document = page,
           term = word,
           value = n)


# 최적 토픽수 찾기
# 토픽모델 만들기
set.seed(123)

topic_models <- data_frame(K = c(5, 10, 15, 20, 30)) %>%
  mutate(topic_model = future_map(K, ~stm(word_dfm,
                                          K = .,
                                          verbose = F)))
topic_models

# 토픽모델 평가
heldout <- make.heldout(word_dfm)

topic_result <- topic_models %>%
  mutate(semantic_coherence = map(topic_model, 
                                  semanticCoherence, 
                                  word_dfm),
         eval_heldout = map(topic_model, 
                            eval.heldout, 
                            heldout$missing),
         residual = map(topic_model, 
                        checkResiduals, 
                        word_dfm),
         bound =  map_dbl(topic_model, 
                          function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, 
                         function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, 
                              function(x) length(x$convergence$bound)))
topic_result

# K: 토픽의 개수
# topic_model: 토픽 모델 객체
# semantic_coherence: 토픽 모델의 의미 일관성 지표
# eval_heldout: 테스트 데이터 세트에 대한 평가 결과
# residual: 토픽 모델의 잔차
# bound: 모델링 과정에서 최대화되는 경계값
# lfact: 토픽의 개수에 따른 lfactorial 값
# lbound: 경계값과 lfactorial 값을 합한 값
# iterations: 최적화 과정에서 수행된 반복 횟수


# 토픽분석결과 그래프
topic_result %>%
  transmute(K,
            Lower_bound = lbound,
            Residuals = map_dbl(residual, 
                                "dispersion"),
            Semantic_coherence = map_dbl(semantic_coherence, 
                                         mean),
            Heldout_likelihood = map_dbl(eval_heldout, 
                                         "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(mapping = aes(x = K, 
                       y = Value, 
                       color = Metric)) +
  geom_line(size = 1,
            show.legend = FALSE) +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(x = "토픽수",
       y = NULL)


# best 토픽 결정
# 최적 토픽 결정
best_model <- topic_result %>% 
  filter(K == 10) %>% 
  pull(topic_model) %>% 
  .[[1]] # 리스트로 되어 있는 부분 제거

# 토픽별 단어 확인
summary(best_model)

# beta 특정 토픽에서 특정단어가 생성될 확률(beta)
# 토픽별 beta 확인
word_beta <- tidy(best_model, 
                  matrix = "beta")
word_beta


# 토픽별 beta 그래프 
word_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  mutate(topic = paste("Topic", topic)) %>%
  ggplot(mapping = aes(x = beta, 
                       y = reorder_within(term, beta, topic), 
                       fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  labs(y = NULL)

# topic별 beta 상대비교
beta_wide <- word_beta %>%
  group_by(topic) %>%
  slice_max(beta, n=10) %>%
  ungroup() %>%
  arrange(topic, -beta)

beta_wide %>%
  mutate(topic = paste0("topic", topic)) %>% # 토픽번호 수정 1->topic1
  pivot_wider(names_from = topic, 
              values_from = beta) %>%
  print(n=74)


# 특정 문서가 특정 토픽을 반영할 확률 (gamma)
# gamma 확인
word_gamma <- tidy(best_model,
                   matrix = "gamma",
                   document_names = rownames(word_dfm))
word_gamma

# 문서별 관련 토픽 그래프
word_gamma %>%
  mutate(docement = fct_reorder(document, gamma),
         topic = factor(topic)) %>%
  ggplot(mapping = aes(x = gamma, 
                       y = topic, 
                       fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(docement), ncol = 4) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL) 

# 문서별 관련 토픽 확률
word_gamma %>%
  mutate(topic = paste0("topic", topic)) %>% # 토픽번호 수정 1->topic1
  pivot_wider(names_from = topic, 
              values_from = gamma) %>%
  print(n=35)

# 원하는 문서만 확인
word_gamma %>%
  mutate(topic = paste0("topic", topic)) %>% # 토픽번호 수정 1->topic1
  pivot_wider(names_from = topic, 
              values_from = gamma) %>%
  filter(document == 1)


# 토픽별 단어, 문서 분포 통합
# beta를 이용해 상위 7개 단어 추출
top_terms <- word_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest() # unnest()에서 수정함

# gamma를 이용해 토픽별 분포 확인
gamma_terms <- word_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>% # 토픽별 gamma 평균
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

# beta와 gamma 통합 그래프
gamma_terms %>%
  ggplot(mapping = aes(x = topic, 
                       y = gamma, 
                       label = terms, 
                       fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.3)) +
  labs(x = NULL)



# ===  === #
