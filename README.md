## 1. 텔레마케팅 이후, 신규 정기예금 가입 여부 분류 모델 구현

  - Member : 김경록(R, SAS 코드 작성 및 보고서 작성), 팀원(ppt 제작, 파생변수 아이디어 제공)
  - Status : Complete
  - Tag : Toy Project
  - 사용언어 / 핵심 라이브러리 : SAS, R / ggplot2, dplyr 등

## 2. Why

은행들은 다양한 마케팅 방법으로 고객들을 유치하고 신규 상품을 가입 시키려고 합니다.

요즘은 어플을 통한 홍보도 있지만, 가장 고객에게 접근하기 쉬운 방식은 텔레마케팅입니다.

젋은 층의 경우, 이를 차단하거나 바로 끊어버리는 등으로 효과가 안 좋다고 알려져있는데

실제로 그러한지, 그리고 효과가 있는지를 kaggle의 데이터를 통해 살펴보려고 합니다.

## 3. Data

[Kaggle 데이터] 포르투갈 은행 텔레 마케팅 성과 자료

## 4. 분석 방법

(a). Data Preprocessing

	- EDA : 고객 개인 정보, 텔레마케팅 기록

	- 시각화 : 독립변수 & 반응변수 누적 바 그래프, 핵심변수 histogram, boxplot

	- Data Reduction & 변수 그룹화 : 과도한 은행 잔고, 비정상적 개인정보 제거 / 나이 -> 나이대 그룹화, 직업군 통일화, date 정보 분리

(b). Model & Algorithms

	- 이전 캠페인 참여/비참여에 따른 신규 참여율 많이 다름 --> 해당 변수 기준, 데이터 분리 후 모델 2개 생성

	- 반응변수 불균형 --> 모델링 과정에서 데이터 비율별 가중치 별도 부여

	- 로지스틱 회귀분석 --> 정확도 계산(이전 참여 : 73%, 이전 미 참여 : 64퍼)

(c). Report & Review

	- 이전 캠페인 참여 여부 & 신규 정기예금 가입 여부 상관성 확인 후, 데이터 분리 --> 향상된 분류 모델 구축

	- 다양한 시각화와 변수 변형을 통해서 Reduction 되어야될 데이터 확인 후 제거

	- 피드백 : 데이터 불균형인 상황에서 최적의 threshold 찾는 과정 없이 단순 0.5로 수행

## cf. kaggle 관련 게시물

kaggle에 있는 원본 데이터 : https://www.kaggle.com/yufengsui/portuguese-bank-marketing-data-set

kaggle에 올린 시각화 노트북 : https://www.kaggle.com/bluemumin/bank-data-eda
