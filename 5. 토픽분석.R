### 토픽분석

# 1. 기본 package 설치
# install.packages("tm")
# install.packages("quanteda")

library(quanteda)
library(tidyverse)
library(tidytext)
library(tm)


# 2. 문서 단어 행렬



# 3-1 tf-idf 만들기

word_t_idf <- word_t_tb %>%
  count(no, word, sort = T) %>%
  bind_tf_idf(word, no, n)


# 3-2 dfm 만들기
word_dfm <- word_t_idf %>%
  cast_dfm(document = no,
           term = word,
           value = n)


# 4. 최적 토픽수 찾기
# install.packages("stm")
library(stm)
library(furrr)


# 토픽모델 만들기
topic_models <- data_frame(K = c(5, 10, 15, 20, 25, 30, 35, 40)) %>%
  mutate(topic_model = future_map(K, ~stm(word_dfm,
                                          K = .,
                                          verbose = F)))
topic_models


# 만들어진 토픽 모델
# 5 <STM>      
# 10 <STM>      
# 15 <STM>      
# 20 <STM>      
# 25 <STM>      
# 30 <STM>
  

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

#semantic_coherence도 있다. 언더바가 없는 것으로 사용한다.


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



# 3.best 토픽 결정

# 최적 토픽 결정
best_model <- topic_result %>% 
  filter(K == 10) %>% 
  pull(topic_model) %>% 
  .[[1]]                    # 리스트로 되어 있는 부분 제거

# 토픽별 단어 확인
summary(best_model)






# 4.beta 특정 토픽에서 특정단어가 생성될 확률(beta)

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






# 5. 특정 문서가 특정 토픽을 반영할 확률 (gamma)

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




# 6.토픽별 단어, 문서 분포 통합

# install.packages("ggthemes") 
library(ggthemes)

# beta를 이용해 상위 7개 단어 추출
top_terms <- word_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

# gamma를 이용해 토픽별 분포 확인
gamma_terms <- word_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%    # 토피별 gamma 평균
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
