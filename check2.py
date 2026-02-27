path=r'C:\\Users\\flori\\OneDrive\\Documents\\AppDesign\\SleepTempFinder\\RScript\\SleepTempFinder.R'
cum=0
maxcum=0
maxline=0
with open(path,'r',encoding='utf-8') as f:
    for i,line in enumerate(f,1):
        for c in line:
            if c=='{': cum+=1
            elif c=='}': cum-=1
        if cum>maxcum:
            maxcum=cum
            maxline=i
print('max cum',maxcum,'on line',maxline,'final cum',cum)

