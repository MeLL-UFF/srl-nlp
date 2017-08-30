# SRL-NLP

This project aims to process structured representations of texts through SRL to boost tasks such _Open Domain Question Answering_, _Semantic Text Similarity_ and _Recognizing Textual Similarity_

## Requirements
* Boxer
* C&C Tools
* Python 2
* Problog, ProbFoil > 2.1
* Aleph

## Installing Requirements
* Problog, ProbFoil (I recommend pip for this task)
    ```shell
    sudo pip install -r requirements.txt
    ```
* Boxer and C&C Tools
    Run make XXX
    #TODO: makefile that installs boxer and the C&C Tools
* If you want to run all the test units, install Pytest and then run it:
```shell
    sudo pip install pytest
    pytest
```
