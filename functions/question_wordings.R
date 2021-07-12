question_wordings <-
    c("CFQ1 | Do you have problems with tiredness?",
      "CFQ2 | Do you need to rest more?",
      "CFQ3 | Do you feel sleepy or drowsy?",
      "CFQ4 | Do you have problems starting things?",
      "CFQ5 | Do you start things without difficulty but get weak as you go on?",
      "CFQ6 | Are you lacking in energy?",
      "CFQ7 | Do you have less strength in your muscles?",
      "CFQ8 | Do you feel weak?",
      "CFQ9 | Do you have difficulty concentrating?",
      "CFQ10 | Do you have problems thinking clearly?",
      "CFQ11 | Do you make slips of the tongue when speaking?",
      "PF1 | Vigorous activities, such as running, lifting heavy objects, vacuum cleaner, bowling, or playing golf",
      "PF2 | Moderate activities, such as moving a table, pushing a vacuum cleaner, bowling, or playing golf",
      "PF4 | Climbing several flights of stairs",
      "PF5 | Climbing one flight of stairs",
      "PF7 | Walking more than a mile",
      "PF8 | Walking several hundred yards",
      "PF10 | Bathing or dressing yourself",
      "SIQ1 | I am afraid that I will make my symptoms worse if I exercise.",
      "SIQ2R | My symptoms would be relieved if I were to exercise (reversed).",
      "SIQ3 | Avoiding unnecessary activities is the safest thing I can do to prevent my symptoms from worsening.",
      "SIQ5R | Even though I experience symptoms, I don't think they are actually harming me (reversed).",
      "SIQ7 | Physical activity makes my symptoms worse.",
      "SIQ8 | Doing less helps symptoms.",
      "SIQ9 | Symptoms are a sign that I am damaging myself.",
      "SIQ10 | I am afraid I will have more symptoms if I am not careful.",
      "SIQ11 | I should avoid exercise when I have symptoms.",
      "SIQ18 | When I experience symptoms, I think about them constantly.",
      "SIQ20 | When I am experiencing symptoms it is difficult for me to think of anything else.",
      "SIQ22 | My symptoms are always at the back of my mind.",
      "SIQ23 | I spend a lot of time thinking about my illness.",
      "SIQ24 | I am embarrassed about my symptoms",
      "SIQ25 | I worry that people will think badly of me because of my symptoms.",
      "SIQ26 | The embarrassing nature of my symptoms prevents me from doing things.",
      "SIQ27 | I avoid social situations because I am scared my symptoms will get out of control.",
      "SIQ28 | I am ashamed of my symptoms.",
      "SIQ29 | My symptoms have the potential to make me look foolish in front of other people.",
      "SIQ30 | I stay in bed to control my symptoms.",
      "SIQ31 | When I experience symptoms, I rest.",
      "SIQ32 | I tend to avoid activities that make my symptoms worse.",
      "SIQ33 | I tend to nap during the day to control my symptoms.",
      "SIQ34 | I tend to overdo things when I feel energetic.",
      "SIQ38 | I sleep when I'm tired in order to control my symptoms.",
      "SIQ39 | I avoid making social arrangements in case I'm not up to it.",
      "SIQ40 | I avoid exerting myself in order to control my symptoms.",
      "SIQ42 | I avoid stressful situations.",
      "WSAQ1 | Because of my [problem] my ability to work is impaired. '0' means 'not at all impaired' and ‘8’ means very severely impaired to the point I can't work.",
      "WSAQ2 | Because of my [problem] my home management (cleaning, tidying, shopping, cooking, looking after home or children, paying bills) is impaired.",
      "WSAQ3 | Because of my [problem] my social leisure activities (with other people e.g.  parties, bars, clubs, outings, visits, dating, home entertaining) are impaired.",
      "WSAQ4 | Because of my [problem], my private leisure activities (done alone, such as reading, gardening, collecting, sewing, walking alone) are impaired.",
      "WSAQ5 | Because of my [problem], my ability to form and maintain close relationships with others, including those I live with, is impaired.")

# I'm assuming that PF1 corresponds to SF-36 question 3, and so on:

# PF1  | SF36 Q3. Vigorous activities, such as running, lifting heavy objects, vacuum cleaner, bowling, or playing golf 
# PF2  | SF36 Q4. Moderate activities, such as moving a table, pushing a vacuum cleaner, bowling, or playing golf
# PF3  | SF36 Q5. Lifting or carrying groceries 
# PF4  | SF36 Q6. Climbing several flights of stairs 
# PF5  | SF36 Q7. Climbing one flight of stairs 
# PF6  | SF36 Q8. Bending, kneeling, or stooping 
# PF7  | SF36 Q9. Walking more than a mile 
# PF8  | SF36 Q10. Walking several blocks 
# PF9  | SF36 Q11. Walking one block 
# PF10 | SF36 Q12. Bathing or dressing yourself 
