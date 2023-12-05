# Web Crawling (네이버 기사)

# 1. 기본 package 설치
# install.packages("")

library(tidyverse)
library(rvest)


# 2-1. 검색어 설정
# 검색어: ChatGpt

search_url <- "https://search.naver.com/search.naver?where=news&sm=tab_jum&query=ChatGPT"


# 2-2. URL 주소 검색
# "#": id
# ".": class
# "na": 태그
# href: 하이퍼링크 레퍼런스의 약자 
# Copy selector
# news_url <- "https://n.news.naver.com/mnews/article/025/0003279066?sid=104"

news_url <- search_url %>%
  read_html() %>%
  # sp_nws1 > div > div > div.news_info > div.info_group > a:nth-child(3)" 앞부분 제거
  html_nodes("div.info_group > a:nth-child(3)") %>%
  html_attr("href") # 필요한 링크 속성값을 추출

news_url


# 2-3. 기사 데이터 저장
result_tb <- NULL
result_tb

for (i in 1:length(news_url))
{
  news_html <- news_url[i] %>%
    read_html()
  
  # 기사 제목
  title <-news_html %>%
    html_nodes('.media_end_head_headline') %>%
    html_text()
  
  # 기사 본문
  body <-news_html %>%
    html_nodes('#newsct_article') %>%
    html_text()
  
  # 언론사
  media <- news_html %>%
    html_nodes("img.media_end_head_top_logo_img.light_type") %>%
    html_attr("title")
  
  # 테이블에 데이터 저장
  temp <- tibble(no = i,
                 출판사 = media,
                 제목 = title,
                 본문 = body)
  result_tb <- rbind(result_tb, temp) # 데이터 업데이트
}

result_tb


# 3-1. 검색어 여러 페이지 설정
# 검색어: ChatGpt
# to: 가져올 데이터로, 네이버는 데이터를 가져올 수 있는 데이터를 4000개로 제한했다.
# 데이터를 최대한 가져올 수 있으나, 형태소 분석 과정에서 긴 시간이 걸린다.
# by: 몇 칸씩 뛸 것인지를 정하는 것으로, default는 1이다. 네이버는 기준이 10이다.
# str_c: stream 함수로, 글자나 텍스트를 정리하여 분석할 때 사용하는 함수이다.

search_url <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query=ChatGPT&sort=0&photo=0&field=0&pd=0&ds=&de=&cluster_rank=46&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:all,a:all&start=" # 뒤의 숫자 제거

page <- seq(from = 1, to = 100, by = 10)
page

search_url <- str_c(search_url, page) # 40페이지까지 가능
search_url


# 3-2. URL 주소 여러 페이지 검색
news_url <- NULL

for(i in 1:length(search_url))
{
  temp <- search_url[i] %>%
    read_html() %>%
    html_nodes("div.info_group > a:nth-child(3)") %>%
    html_attr("href")
  
  news_url <- c(news_url, temp)
}

news_url


# 3-3. 기사 데이터 저장
# n.news가 아닌 sports의 경우 자동으로 제외된다.
# 일반적인 링크: https://n.news.
# 제외되는 링크: https://sports.

result_tb <- NULL
result_tb

for (i in 1:length(news_url))
{
  news_html <- news_url[i] %>%
    read_html()
  
  # 기사 제목
  title <-news_html %>%
    html_nodes('.media_end_head_headline') %>%
    html_text()
  
  # 기사 본문
  body <-news_html %>%
    html_nodes('#newsct_article') %>%
    html_text()
  
  # 언론사
  media <- news_html %>%
    html_nodes("img.media_end_head_top_logo_img.light_type") %>%
    html_attr("title")
  
  # 테이블에 데이터 저장
  temp <- tibble(no = i,
                 출판사 = media,
                 제목 = title,
                 본문 = body)
  result_tb <- rbind(result_tb, temp) # 데이터 업데이트
}

result_tb


# 3-4. 결과 내보내기
# 테이블 형식으로 저장될 수 있게끔 csv가 아닌 excel_csv로 저장

result_tb %>%
  write_excel_csv("Article content analysis.csv")
result_tb





# 4-1. 검색어 조건 페이지 설정
# 정렬: 최신순 (관련도순, 최신순, 오래된순)
# 기간: 2023.04.01. ~ 2023.04.30.
# 유형: 지면기사 (전체, 포토, 동영상, 지면기사, 보도자료, 자동생성기사)
# 언론사: 전체
# 2번 페이지로 넘어가서 주소 복사

search_url <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query=ChatGPT&sort=1&photo=3&field=0&pd=3&ds=2023.04.01&de=2023.04.30&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:dd,p:from20230401to20230430,a:all&start=" # 뒤의 숫자 제거

page <- seq(from = 1, to = 100, by = 10)
page

search_url <- str_c(search_url, page)
search_url


# 4-2. URL 주소 조건 페이지 검색
news_url <- NULL

for(i in 1:length(search_url))
{
  temp <- search_url[i] %>%
    read_html() %>%
    html_nodes("div.info_group > a:nth-child(4)") %>% # 조건이 생성되면서 3->4로 변경
    html_attr("href")
  
  news_url <- c(news_url, temp)
}

news_url


# 3-3. 기사 조건 데이터 저장

result_tb <- NULL
result_tb

for (i in 1:length(news_url))
{
  news_html <- news_url[i] %>%
    read_html()
  
  # 기사 제목
  title <-news_html %>%
    html_nodes('.media_end_head_headline') %>%
    html_text()
  
  # 기사 본문
  body <-news_html %>%
    html_nodes('#newsct_article') %>%
    html_text()
  
  # 언론사
  media <- news_html %>%
    html_nodes("img.media_end_head_top_logo_img.light_type") %>%
    html_attr("title")
  
  # 테이블에 데이터 저장
  temp <- tibble(no = i,
                 출판사 = media,
                 제목 = title,
                 본문 = body)
  result_tb <- rbind(result_tb, temp) # 데이터 업데이트
}

result_tb

