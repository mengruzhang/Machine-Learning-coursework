load faces.mat

%1.2.1
svmModel=trainSVM(traindata, trainlabels, 500);
testLabels = classifySVM(svmModel, testdata);
error = sum(testLabels' ~= testlabels)/size(testdata,1)

%1.2.2
%please refer to cvSVM.m

%1.2.3
cvErrorSVM = zeros(8,1);
trainErrorSVM = zeros(8,1);
testErrorSVM = zeros(8,1);

C = [10, 10^2, 10^3, 10^4, 5*10^4, 10^5, 5*10^5, 10^6];
for k = 1: 8
    %get train.cv.error
    cvErrorSVM(k) = cvSVM(C(k),traindata,trainlabels,10);
    
    %get SVM model of C(k)
    tempSVM = trainSVM(traindata, trainlabels, C(k));
    
    %train error
    trainPred = classifySVM(tempSVM, traindata);
    trainErrorSVM(k) = sum(trainPred' ~= trainlabels)/size(trainlabels,1);
    
    %test error
    testPred = classifySVM(tempSVM, testdata);
    testErrorSVM(k) = sum(testPred' ~= testlabels)/size(testlabels,1);
end
                        
cvErrorSVM
trainErrorSVM
testErrorSVM

