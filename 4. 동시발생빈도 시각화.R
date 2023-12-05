# install.packages("igraph")
# install.packages("ggraph")
# install.packages("tidygraph")

library(igraph)
library(ggraph)
library(tidygraph)

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
  write_excel_csv("word_pairs_graph.xls")



# 4. 그래프 그리기

# 4.1 기본그래프
# 기본테이블로 그리기
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



# 연결중심성으로 그리기
word_pairs_graph %>%
  as_tbl_graph(directed = F) %>%
  ggraph(layout = "fr") +
  geom_edge_link() +
  geom_node_point(mapping = aes(size = cent_dgr)) +
  geom_node_text(mapping = aes(label = name),
                 vjust = 1,
                 hjust = 1)




# 4.2 그래프 그리기 (색)
# 연결중심으로 그리기

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




# pagelink로 그리기
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




# 4.3 여러 색으로 그래프 그리기
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





# 5. 하위단어 분석
# 학습과 가장 많이 나타나는 단어와 상관관계가 높은 단어
word_pairs %>%
  filter(item1 == "chatgpt")

word_pairs %>%
  filter(item1 %in% c("chatgpt", "인공지능")) %>%
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

# show.legend = FALSE: 범례 삭제
# scales = "free"): sclaes를 free로 하므로 별개로 나온다
# 상위 50위 이내에서 보여주는 것이 좋다. (단어 개수는 50~100개)





word_cors %>%
  filter(item1 == "chatgpt")

word_cors %>%
  filter(item1 %in% c("chatgpt", "인공지능")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 20) %>%
  ungroup() %>%
  mutate(item2 = reorder_within(item2, correlation, item1)) %>%
  ggplot(aes(x = correlation, 
             y = item2, 
             fill = item1)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ item1, ncol = 2, scales = "free") +
  scale_y_reordered()