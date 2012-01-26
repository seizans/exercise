
データ型 Tree、
葉の個数を数える関数 leaf、
節の個数を数える関数 node、
をそれぞれ以下のように定義する。

> data Tree = Leaf Int | Node Tree Tree

> leaf :: Tree -> Int
> leaf (Leaf _) = 1
> leaf (Node t1 t2) = leaf t1 + leaf t2

> node :: Tree -> Int
> node (Leaf _) = 0
> node (Node t1 t2) = 1 + node t1 + node t2

この Tree において、葉の個数が節の個数よりも常に1多い、即ち、
t :: Tree
leaf t = 1 + node t
が常に成り立つことを数学的帰納法で示す。

(1) t = Leaf x の場合
  leaf (Leaf x) = 1
  node (Leaf x) = 0
となって成立。

(2) t = t1、t = t2 について仮定が成立したとして、t = Node t1 t2 の場合
  leaf (Node t1 t2) = leaf t1 + leaf t2
                    = (1 + node t1) + (1 + node t2)
                    = 1 + (1 + node t1 + node t2)
                    = 1 + node (Node t1 t2)
となって成立。

よって(1)、(2)より示された。
