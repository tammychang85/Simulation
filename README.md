# Simulation

所有論文相關的程式碼以及產出的結果(資料、圖檔)都在 Simulation 夾中，包含：
1. Simulation.Rproj：R project 的檔案
2. Functions.R：存放所有模擬用到的套件以及函式(欲進行新的一輪模擬時須先跑過該檔案，若選擇直接使用舊有的資料則不用)
3. Simulation.R：用來進行模擬以及產出圖表
4. BuildScenarioTree.R：用來建構 Neural gas tee 的函示，為原套件函式的修改版，可以建造只含有1條 scenario path 的 Neural gas tree
5. results 資料夾：依模擬日期存放模擬結果的資料
6. realizations 資料夾：依模擬日期存放模擬用的訓練集
7. testSets 資料夾：依模擬日期存放模擬用的測試集
8. graphs 資料夾：存放模擬結果的圖

**其中每個資料夾皆含有一個日期為 "0619" 的子資料夾，裡面存放的為論文使用的資料**


## results 資料夾詳細說明
點進以日期命名的子資料夾後應包含 20 個 rds 檔，分別為：

* highFixedOrders.rds 高懲罰成本結構下 Neural gas tree 向 fixed 供應商訂購的數量
* highFlexibleOrders.rds 高懲罰成本結構下 Neural gas tree 向 flexible 供應商訂購的數量
* highLargeCost.rds 高懲罰成本結構下有32條 sceanrio path 的 Neural gas tree 的成本表現
* highNeuralCosts.rds 高懲罰成本結構下(二元的)Neural gas tree 的成本表現
* highResidualCosts2.rds 高懲罰成本結構下 bin=2 的 Residual tree 的成本表現
* highResidualCosts4.rds 高懲罰成本結構下 bin=4 的 Residual tree 的成本表現
* highResidualCosts5.rds 高懲罰成本結構下 bin=5 的 Residual tree 的成本表現
* highSingleCost.rds 高懲罰成本結構下只1條 sceanrio path 的 Neural gas tree 的成本表現
* lowFixedOrders.rds 低懲罰成本結構下 Neural gas tree 向 fixed 供應商訂購的數量
* lowFlexibleOrders.rds 低懲罰成本結構下 Neural gas tree 向 flexible 供應商訂購的數量 
* lowLargeCost.rds 低懲罰成本結構下有32條 sceanrio path 的 Neural gas tree 的成本表現
* lowNeuralCosts.rds 低懲罰成本結構下(二元的)Neural gas tree 的成本表現
* lowResidualCosts2.rds 低懲罰成本結構下 bin=2 的 Residual tree 的成本表現
* lowResidualCosts4.rds 低懲罰成本結構下 bin=4 的 Residual tree 的成本表現
* lowResidualCosts5.rds 低懲罰成本結構下 bin=5 的 Residual tree 的成本表現
* lowSingleCost.rds 低懲罰成本結構下只1條 sceanrio path 的 Neural gas tree 的成本表現
* neuralTime.rds Neural gas tree 的運算時間
* residualTime2.rds Residual tree bin=2 的運算時間
* residualTime4.rds Residual tree bin=4 的運算時間
* residualTime5.rds Residual tree bin=5 的運算時間

#### 補充
reulsts/0619 資料夾額外多了 highCost.rds & lowCost.rds 兩個資料(value of flexibility 的部分用來計算 cost ratio 的)。

這兩個資料是 Neural gas tree 分別在高低懲罰成本下的 cost，因當初論文測試到這部份時時重新建了一棵樹去使用而產生，

但程式碼重新整合後則改為直接使用前面已建構完的樹(產生的資料為 highNeuralCost.rds & lowNeuralCost.rds) 計算 cost ratio，

為了整合舊有的資料與新程式碼的邏輯，將程式設定成選用 0619 的資料夾時(選擇方法下面會提到)會特別載入這兩個資料去計算 cost ratio 成本(在 value of flexibility 的部分)，

若選擇重新模擬則不會產生這兩個資料。

                                                                                                       
## Simulation.R 詳細說明
共分為 simulation & visualization 兩大部分：

### simulation
負責實驗模擬，可以設定 **simulationMode** 這個參數來決定模擬的模式。**若 simulationMode 設為 TRUE(預設)**，
則程式會直接自 results 資料夾讀取結果的資料不進行新的模擬，**預設讀取 0619**，即論文使用的資料(只讀取結果的資料，不讀取訓練集和測試集)，也可以修改 **resultFilePath** 參數自行選擇讀取哪天的資料夾。

若 **simulationMode 設為 FALSE**，則程式會進行新的模擬，並將結果依日期儲存在資料夾，**注意若要進行新的模擬需先跑過 Function.R 檔載入需要的套件與函式**。

### visualization
負責資料視覺化以及各種檢定、區間計算，**在 simulation 部分執行完取得資料後即可使用**。
