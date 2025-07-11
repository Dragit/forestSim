# forestSim

## Oh, hi Marc

**Simple tool to watch Random Forests learn — step by step!**

---

### What is it?

`forestSim` helps you see how a random forest’s accuracy (or error) improves as it grows trees. It works for both classification and regression — and even tests on separate data!

---

### Can I just run whatever is there ?

Sure, just navigate to `./abgabe/ExerciseSheet_6_TEMPLATE.Rmd` and then run the code blocks inside R studio!  
This will rebuild the r package and then show you what the basic capabilities are.


---

### How to use yourself?

1. Install and load:

    ```r
    devtools::install("./forestSim")
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

If you want to learn more, just ask! 🌲📈
