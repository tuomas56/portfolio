##Theorem
$$f(n) = 1\cdot2903^n + -1\cdot803^n + -1\cdot464^n + 1\cdot261^n$$ is divisible by $1897$.
####Proof
Let $P(n)$ be the proposition that $$f(n) = 1\cdot2903^n + -1\cdot803^n + -1\cdot464^n + 1\cdot261^n$$ is divisible by $1897$.

For $P(1)$: $$1897 = 1 \cdot 1897$$

For $P(n + 1)$: $$f(n + 1) = 261\cdot f(n) + 2642\cdot2903^n + -542\cdot803^n + -203\cdot464^n$$
Thus $P(n) \rightarrow P(n + 1)$ by lemma 1.
Thus $P(n)$ for $n > 0$ by induction.


###Lemma 1
$$f(n) = 2642\cdot2903^n + -542\cdot803^n + -203\cdot464^n$$ is divisible by $1897$.
####Proof
Let $P(n)$ be the proposition that $$f(n) = 2642\cdot2903^n + -542\cdot803^n + -203\cdot464^n$$ is divisible by $1897$.

For $P(1)$: $$7140308 = 3764 \cdot 1897$$

For $P(n + 1)$: $$f(n + 1) = 464\cdot f(n) + 6443838\cdot2903^n + -183738\cdot803^n$$
Thus $P(n) \rightarrow P(n + 1)$ by lemma 2.
Thus $P(n)$ for $n > 0$ by induction.


###Lemma 2
$$f(n) = 6443838\cdot2903^n + -183738\cdot803^n$$ is divisible by $1897$.
####Proof
Let $P(n)$ be the proposition that $$f(n) = 6443838\cdot2903^n + -183738\cdot803^n$$ is divisible by $1897$.

For $P(1)$: $$18558920100 = 9783300 \cdot 1897$$

For $P(n + 1)$: $$f(n + 1) = 803\cdot f(n) + 13532059800\cdot2903^n$$
Thus $P(n) \rightarrow P(n + 1)$ by lemma 3.
Thus $P(n)$ for $n > 0$ by induction.


###Lemma 3
$$f(n) = 13532059800\cdot2903^n$$ is divisible by $1897$.
####Proof
Let $P(n)$ be the proposition that $$f(n) = 13532059800\cdot2903^n$$ is divisible by $1897$.

For $P(n)$: $$39283569599400 = 20708260200 \cdot 1897$$


QED.
