import numpy as np
import torch
import torch.nn.functional as F
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import DataLoader
import tqdm

if torch.cuda.is_available():
    print("The code will run on GPU. This is important so things run faster.")
else:
    print("The code will run on CPU. You should probably not do this.")
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

class dataset:
    def __init__(self,data,target):
        self.data = data
        self.target = target
    
    def __len__(self):
        return len(self.data)
    
    def __getitem__(self,idx):
        if torch.is_tensor(idx):
            idx = idx.tolist()
        X = self.data[idx]
        y = self.target[idx]
        
        return X,y
    
#We define the training as a function so we can easily re-use it.
def train(model, optimizer, num_epochs=10):
    train_acc_all = []
    test_acc_all = []

    for epoch in range(num_epochs):
        model.train()
        #For each epoch
        train_correct = 0
        for minibatch_no, (data, target) in enumerate(train_loader):
            data, target = data.to(device), target.to(device)
            #Zero the gradients computed for each weight
            optimizer.zero_grad()
            #Forward pass your image through the network
            output = model(data)
            #Compute the loss
            loss = F.nll_loss(torch.log(output), target)
            #Backward pass through the network
            loss.backward()
            #Update the weights
            optimizer.step()

            #Compute how many were correctly classified
            predicted = output.argmax(1)
            train_correct += (target==predicted).sum().cpu().item()
        #Comput the test accuracy
        test_correct = 0
        model.eval()
        for data, target in test_loader:
            data = data.to(device)
            with torch.no_grad():
                output = model(data)
            predicted = output.argmax(1).cpu()
            test_correct += (target==predicted).sum().item()
        train_acc = train_correct/len(trainset)
        test_acc = test_correct/len(testset)
        train_acc_all.append(train_acc)
        test_acc_all.append(test_acc)
    return test_acc_all, train_acc_all

class LinearNet(nn.Module):
    def __init__(self,p):
        super(LinearNet, self).__init__()
        
        self.linear = nn.Sequential(nn.Linear(in_features=300,out_features=150),
                                    nn.ReLU(),
                                    nn.Dropout(p=p),
                                    nn.Linear(150,75),
                                    nn.ReLU(),
                                    nn.Dropout(p=p),
                                    nn.Linear(75,10),
                                    nn.Softmax(dim=1))
                                      

        
    def forward(self, x): #x = [batch_size,100,3]
        x = x.view(x.shape[0],-1)
        x = self.linear(x)
        return x

data = np.load("armdata.npy")
data = data.reshape(100,100,3)
target = [0]*10+[1]*10+[2]*10+[3]*10+[4]*10+[5]*10+[6]*10+[7]*10+[8]*10+[9]*10
target = np.array(target)
data = torch.from_numpy(data).float()
target = torch.from_numpy(target).long()

test_acc_all = []
train_acc_all = []

batch_size=10

for i in tqdm.tqdm(range(100),unit="model"):
    test_idx = torch.tensor([i])
    train_idx = np.delete(np.arange(100),test_idx)
    data_test = data[test_idx]
    data_train = data[train_idx]
    target_test = target[test_idx]
    target_train = target[train_idx]

    trainset = dataset(data_train,target_train)
    testset = dataset(data_test,target_test)

    train_loader = DataLoader(trainset, batch_size=batch_size, shuffle=True, num_workers=0)
    test_loader = DataLoader(testset, batch_size=batch_size, shuffle=False, num_workers=0)
    
    model = LinearNet(p=0.15)
    model.to(device)
    lr = 1e-4
    optimizer = optim.Adam(model.parameters(), lr=lr)
    test_acc, train_acc = train(model, optimizer,num_epochs=650)
    test_acc_all.append(test_acc)
    train_acc_all.append(train_acc)
    
test_acc_final = np.array(test_acc_all)
train_acc_final = np.array(train_acc_all)

np.save("test_acc_final",test_acc_final)
np.save("train_acc_final",train_acc_final)