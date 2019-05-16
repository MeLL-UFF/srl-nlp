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
* spaCy
  - en_core_web_sm model

## Installing Requirements
* Problog, ProbFoil Distance config parser...(I recommend pip for this task)
    ```shell
        sudo pip install -r requirements.txt
    ```
* Aleph
    ```shell
        wget http://www.comlab.ox.ac.uk/oucl/research/areas/machlearn/Aleph/aleph.pl -O ../aleph.pl
    ```
* Boxer and C&C Tools
    Clone the repository of KNews doing the following and then follow the read-me of that repository:
    ```shell
        (cd ..
        git clone https://github.com/valeriobasile/learningbyreading.git
        )
    ```

### Datasets:
* SemEval07 Task 19
* Framenet 1.X
* Dailymail and CNN

### Experiments dependency
The testing with SemEval07 Task 19 demands FrameNet data and demands all frames to be in a single xml file it can be done by running the script `XXX`, run `path/XX --help` for help on how to use it.

## Code Testing
* If you want to run all the **test units**, install Nose and then run it:
    ```shell
        sudo pip install -U nose
        pytest
    ```

## Read Also:

## Contact

Besides the issue section in the github, directed for any question related to this repository, you can contact me through [my email](brenocarvalho@ic.uff.br).