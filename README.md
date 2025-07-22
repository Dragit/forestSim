# forestSim

**Simple tool to watch Random Forests learn and get better — step by step, or better tree by tree!**

---

### What is it?

`forestSim` helps you see how a random forest’s accuracy (or error) improves as it grows trees. It works for both classification and regression — and even tests on separate data!

---

### Can I just run whatever is there ?

Sure, just navigate to `./abgabe/ExerciseSheet_6_TEMPLATE.Rmd` and then run the code blocks inside R studio!  
This will rebuild the r package and then show you what the basic capabilities are.


---

### How to develop yourself?

clone the repo and the run the follwing commands in your editor

1. Install and load:

    ```r
    devtools::install("../forestSim")
    library(forestSim)
    ```

2. Run a simulation on your data:

    ```r
    sim <- simulate_forest(your_data, target = "YourTarget", ntree = 50, test = your_test_data)
    ```

3. See the learning progress:

    ```r
    plot_accuracy_growth(sim)
    ```

---

### Why use it?

- Understand how many trees your forest needs  
- Compare training (OOB) vs. test accuracy  
- Watch regression or classification improve over time  

---

Made for learners and quick experiments. No jargon, no fuss.

---

### Usage

See the documentation in R for detailed information

### Usefull Info

1. What is OOB (Out-Of-Bag) error?

   * Each tree in a random forest is trained on a bootstrap sample (~63% of training data).
   * The OOB samples are the ~37% left out from that tree’s training.
   * OOB error is computed by predicting these OOB samples using that tree — no peeking at them during training.
   * It’s an internal, honest error estimate on the training data.

2. What is Test set error?

   * Test set contains data points completely separate and independent from training.
   * However, your test set is usually smaller and might be easier or less variable than the full training data.
   * Also, your test set may be more "homogeneous" or less noisy in some respects.

#### Possible reasons why test accuracy > OOB accuracy even with proper splitting

* Full Forest vs. OOB Predictions
* OOB accuracy is computed tree-by-tree using only the subset of trees that did not see each sample (out-of-bag trees).
* Test accuracy is computed using the entire forest — all trees — making predictions more stable and usually better.

$$
\text{OOB-Accuracy} = 1 - \text{OOB-Error}
$$

In other words:

* OOB is a per-tree partial prediction.
* Test is a full ensemble prediction.

This difference means test predictions generally have lower variance and better accuracy.

* Size and Distribution of Test Set
* Even a random split can lead to some random variation in difficulty.
* If your test subset is smaller or has less variability, accuracy can appear artificially higher.
* Also, if the training data is very noisy or has outliers, OOB error may reflect that noise more strongly.
* OOB Error is a Conservative Estimate
* OOB error is known to be a slightly pessimistic but unbiased estimate of true test error.
* It’s a built-in “cross-validation” but with fewer trees voting per sample.
* Test set evaluation uses all trees for each sample and hence has lower error variance.
