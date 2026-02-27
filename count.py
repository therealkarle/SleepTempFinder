import sys
path=r'C:\\Users\\flori\\OneDrive\\Documents\\AppDesign\\SleepTempFinder\\RScript\\SleepTempFinder.R'
bal={'(':0,')':0,'{':0,'}':0,'[':0,']':0}
with open(path,'r',encoding='utf-8') as f:
    for line in f:
        for c in line:
            if c in bal: bal[c]+=1
print(bal)
