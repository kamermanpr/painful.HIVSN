# Factor analysis and internal consistency

## Overview
Determinatio of the internal consistency and factor structure of isiZulu translations of the Connor-Davidson Resilience Scale (CD-RISC) [(PMID)](http://http://www.ncbi.nlm.nih.gov/pubmed/12964174), and the Resilience Scale (RS) [(PMID)](http://www.ncbi.nlm.nih.gov/pubmed/7850498) administered to 154 (CD-RISC) / 151 (RS) of 197 ambulatory HIV-positive South Africans (the remaining participants opted to completed the orginal English versions). The study took place between September 2014 and March 2015, at the HIV Clinic, Charlotte Maxeke Johannesburg Academic Hospital, South Africa. All participants gave written informed consent (Human Ethics Research Committee, University of the Witwatersrand, South Africa -- clearance no: M140538), and no personal identifying information is provided here. 

The repository contains the [data](./data/), [analysis script](factor.analysis.Rmd), and the associated [markdown document](factor.analysis.md) and [figure](./figures/) outputs. 

The factor structure of the original English questionnaires are shown in the table below. PDF copies of the original papers are located in: [information](./information/).

| Questionnaire | Measurement scale | Analysis | No. of factors | Names given to factors | Questions loading onto each factor |
|---|----|---|---|----------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| CD-RISC | Ordinal 1-5 scale anchored at<br>*‘not true at all’* to<br>*‘true nearly all the time’* | Exploratory Factor<br>Analysis with<br>ORTHOMAX rotation | 5 | **Factor 1:** Personal competence<br>**Factor 2:** Trusting one’s instincts<br>**Factor 3:** Positive acceptance of change<br>**Factor 4:**<br>Control<br>**Factor 5:** Spiritual influences | **Factor 1:**<br>10, 11, 12, 16, 17, 23, 24, 25<br>**Factor 2:**<br>6, 7, 14, 15, 18, 19, 20<br><br>**Factor 3:**<br>1, 4, 5, 2, 8<br><br><br>**Factor 4:**<br>13, 21, 22<br>**Factor 5:**<br>3,9<br><br> |
| Resilience Scale | Ordinal 1-7 scale anchored at<br>*‘strongly disagree’* to *‘strongly agree’* | Principal Component Analysis with OBLIMIN<br>rotation and Kaiser normalisation | 2 | **Factor 1:** Personal competence<br><br>**Factor 2:** Acceptance of self and life | **Factor 1:**<br>1, 2, 3, 4, 5, 6, 9, 10,13, 14, 15, 17, 18, 19, 20, 23, 24<br>**Factor 2:**<br>7, 8, 11, 12, 16, 21, 22, 25 |

Code sheets for the data are provided below.

## Code sheet
### CD-RISC
[**cd.risc.csv**](./data/)  
[_Imported by 'factor.analysis.Rmd'_](.)  

| Label   | Key                                                                                             |
|:--------|:------------------------------------------------------------------------------------------------|
| Q1      | I am able to adapt when changes occur                                                           |
| Q2      | I have at least one close and secure relationship that helps me when I'm stressed               |
| Q3      | When there are no clear solutions to my problems, sometimes fate or God can help                |
| Q4      | I can deal with whatever comes my way                                                           |
| Q5      | Past successes give me confidence in dealing with new challenges and difficulties               |
| Q6      | I try and see the humorous side of things when I'm faced with problems                          |
| Q7      | Having to cope with stress can make me stronger                                                 |
| Q8      | I tend to bounce back after illness, injury or other hardships                                  |
| Q9      | Good or bad, I believe that most things happen for a reason                                     |
| Q10     | I give my best effort no matter what the outcome may be                                         |
| Q11     | I believe I can achieve my goals, even if there are obstacles                                   |
| Q12     | Even when things look hopeless, I don't give up                                                 |
| Q13     | During times of stress/crisis, I know where to turn for help                                    |
| Q14     | Under pressure, I stay focused and think clearly                                                |
| Q15     | I prefer to take the lead in solving problems rather than letting others make all the decisions |
| Q16     | I am not easily discouraged by failure                                                          |
| Q17     | I think of myself as a strong person when dealing with life's challenges and difficulties       |
| Q18     | I can make unpopular or difficult decisions that affect other people, if it is necessary        |
| Q19     | I am able to handle unpleasant or painful feelings like sadness, fear, and anger                |
| Q20     | In dealing with life's problems, sometimes you have to act on a hunch without knowing why       |
| Q21     | I have a strong sense of purpose in life                                                        |
| Q22     | I feel in control of my life                                                                    |
| Q23     | I like challenges                                                                               |
| Q24     | I work to attain my goals no matter what roadblocks I encounter along the way                   |
| Q25     | I take pride in my achievements                                                                 |

### Resilience Scale
[**resilience.scale.csv**](./data/)  
[_Imported by 'factor.analysis.Rmd'_](.)  

| Label   | Key                                                                            |
|:--------|:-------------------------------------------------------------------------------|
| Q1      | When I make plans, I follow through with them                                  |
| Q2      | I usually manage one way or another                                            |
| Q3      | I am able to depend on myself more than anyone else                            |
| Q4      | Keeping interested in things is important to me                                |
| Q5      | I can be on my own if I have to                                                |
| Q6      | I feel proud that I have accomplished things in life                           |
| Q7      | I usually take things in my stride                                             |
| Q8      | I am friends with myself                                                       |
| Q9      | I feel that I can handle many things at a time                                 |
| Q10     | I am determined                                                                |
| Q11     | I seldom wonder what the point of it all is                                    |
| Q12     | I take things one day at a time                                                |
| Q13     | I can get through difficult times because I have experienced difficulty before |
| Q14     | I have self-discipline                                                         |
| Q15     | I keep interested in things                                                    |
| Q16     | I can usually find something to laugh about                                    |
| Q17     | My belief in myself gets me through hard times                                 |
| Q18     | In an emergency, I am someone people can general rely on                       |
| Q19     | I can usually look at a situation in a number of ways                          |
| Q20     | Sometimes I make myself do things whether I want to or not                     |
| Q21     | My life has meaning                                                            |
| Q22     | I do not dwell on things I cannot do anything about                            |
| Q23     | When I am in a difficult situation, I can usually find my way out of it        |
| Q24     | I have enough energy to do what I have to do                                   |
| Q25     | It is OK if there are people who do not like me                                |

