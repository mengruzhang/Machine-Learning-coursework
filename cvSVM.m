function cvError = cvSVM(c,data,labels,n)

d = size(labels,1)/n;
randomIndex = randperm(size(labels,1));
error = 0;

for i = 1:n
    index = randomIndex;
    %get the index of points belong to test group 
    testIndex = index((i-1)*d+1:i*d);
    testData = data(testIndex,:);
    testLabels = labels(testIndex,:);
    
    %get the rest index for train group
    index((i-1)*d+1:i*d) = [];
    trainIndex = index;
    trainData = data(trainIndex,:);
    trainLabels = labels(trainIndex,:);

    %get the svm model of train data
    svmModel = trainSVM(trainData, trainLabels, c);
    predLabels = classifySVM(svmModel, testData);
    
    %get the error count
    error = error + sum(predLabels' ~= testLabels);
    
end

cvError = error/size(labels,1);