# ubi-tw-microsim

台灣無條件基本收入(Unconditional Basic Income, UBI)提案之微觀模擬(Microsimulation)，以靜態分析為方法，評估UBI提案之成效預估。

- 資料來源為行政院主計總處的[家庭收支調查](https://www.dgbas.gov.tw/np.asp?ctNode=2828) (**目前提供的104年及105年資料之計算**)
- 以[徐偉初 2011](http://www.mof.gov.tw/public/Attachment/412111161669.pdf)之研究作為稅負轉嫁和歸宿假設 (**假設B仍在實作中**)

## 使用方法

### 前置作業

1. 申請104年或105年度家庭收支調查
2. 安裝第三方R套件：`foreign`, `ineq`
```R
> install.packages("foreign")
> install.packages("ineq")
```

### 下載

直接藉由`git clone`或是下載[zip檔](https://github.com/SuJiaKuan/ubi-tw-microsim/archive/master.zip)並解壓縮

### 執行

進入資料夾後，看到`main.R`為主要檔案，輸入的參數如下：

- *year*: 資料年份，目前僅能為`104`或是`105`
- *input.path*: 資料檔案位置，提供`.sav`或是`.dta`格式
- *[apportion.hypothesis]*: 歸宿假設，預設為`A`。`B`仍在實作中

可以直接執行script或是在R的互動模式下執行，以下提供資料為105年的執行方式範例：
- 直接執行script
```bash
Rscript main.R 105 inc105.sav
```
- 或是，R的互動模式下執行
```R
> source("main.R")
> main(105, inc105.sav)
```

## 貢獻

### Coding Style
本專案以[Google's R Style Guide](https://google.github.io/styleguide/Rguide.xml)為標準
