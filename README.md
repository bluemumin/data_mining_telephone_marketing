# 포르투갈 텔레마케팅 신규예금 가입 여부 분류

동국대학교 데이터마이닝 수업에서 진행하였던 텔레마케팅을 통한 정기예금 가입자 분류 팀 프로젝트입니다.

kaggle에 있는 원본 데이터 : https://www.kaggle.com/yufengsui/portuguese-bank-marketing-data-set

kaggle에 올린 시각화 노트북 : https://www.kaggle.com/bluemumin/bank-data-eda

- 참여자
 <p> 김경록(R, SAS 코드 작성 및 보고서 작성), 박권수(ppt 제작 및 파생변수 생성) </p>

- 사용언어 / 핵심 라이브러리
 <p> SAS, R / ggplot2, dplyr 등 </p>

- Background 
 <p> 텔레마케팅 성과 확인 및 개선을 위해, 고객 개인정보 및 전화 시간등을 활용하여 신규 정기예금의 가입 여부를 분류</p>

- Summary
	<p>(1). Data Collection <br/>
		- 포르투갈 은행 텔레 마케팅 성과 자료 [kaggle] </p>
	<p>(2). Data Preprocessing <br/>
		- EDA (고객 개인 정보, 텔레마케팅 기록,) <br/>
		- 시각화 (독립변수 & 반응변수 누적 바 그래프, 핵심변수 histogram, boxplot   ) <br/>
		- 변수 변형 (나이 -> 나이대 그룹화, 직업군 통일화, date 정보 분리) <br/>
		- Reduction (과도한 은행 잔고, 비정상적 개인정보 제거) </p>
	<p>(3). Model & Algorithms <br/>
		- 이전 캠페인 참여/비참여에 따른 신규 참여율 많이 다름 --> 해당 변수 기준, 데이터 분리 후 모델 2개 생성 <br/>
		- 반응변수 불균형 --> 모델링 과정에서 데이터 비율별 가중치 별도 부여 <br/>
		- 로지스틱 회귀분석 --> 정확도 계산(이전 참여 : 73%, 이전 미 참여 : 64퍼) </p>
	<p>(4). Report & Review <br/>
		- 이전 캠페인 참여 여부와 신규 정기예금 가입 여부와의 상관성을 확인 후, 데이터 분리하여 향상된 성능의 모델 구축 <br/>
		- 다양한 시각화와 변수 변형을 통해서 Reduction 되어야될 데이터 확인 후 제거 <br/>
		- 피드백 : 데이터 불균형인 상황에서 최적의 threshold 찾는 과정 없이 단순 0.5로 수행 </p>
