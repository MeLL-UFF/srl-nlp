# SRL-NLP

This project aims to process structured representations of texts through SRL to boost tasks such _Open Domain Question Answering_, _Semantic Text Similarity_ and _Recognizing Textual Similarity_

## Requirements
* Boxer
* C&C Tools
* Python 2
* Problog, ProbFoil > 2.1
* Aleph
* Prolog package sgml
* The datasets used in the experimentation (only if you are goig to run the experiments)

## Installing Requirements
* Problog, ProbFoil (I recommend pip for this task)
    ```shell
    sudo pip install -r requirements.txt
    ```
* Boxer and C&C Tools
    Run make XXX
    #TODO: makefile that installs boxer and the C&C Tools

### Datasets:
* SemEval07 Task 19:
* Framenet 1.X
* Dailymail and CNN

### Experiments dependency
The testing with SemEval07 Task 19 demands FrameNet data and demands all frames to be in a single xml file it can be done by running the script `XXX`, run `path/XX --help` for help on how to use it.

## Testing
* If you want to run all the **test units**, install Pytest and then run it:
    ```shell
        sudo pip install -U pytest
        pytest
    ```
