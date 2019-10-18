import sys
import Mecab
import pandas as pd

df=pd.read_csv("mecab.csv",index_col=False,dtype="object")
col=df["statement"]
row=col.iloc[2]
mecab=MeCab.Tagger()
mecab.parseToNode("")
node=mecab.parseToNode("row")

while node:
    print(node.surface,node.feature)
    node=node.next