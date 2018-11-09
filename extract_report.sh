base_folder="/dccstor/sallesd1/breno_tmp/experiments/fn1.5"
out_path="/Users/brenow/google_drive/Mestrado UFF/pics/coverage1"
threshould=3
host="ccc8"


scp ${host}:${base_folder}/current/graphs/coverage${threshould}.png              "${out_path}/cov_current.png"

scp ${host}:${base_folder}/aug_ALL_full/graphs/coverage${threshould}.png         "${out_path}/cov_full_all.png"
scp ${host}:${base_folder}/aug_ALL_lexical/graphs/coverage${threshould}.png      "${out_path}/cov_lexical_all.png"
scp ${host}:${base_folder}/aug_ALL_semantic/graphs/coverage${threshould}.png     "${out_path}/cov_semantic_all.png"
scp ${host}:${base_folder}/aug_ALL_syntactic/graphs/coverage${threshould}.png    "${out_path}/cov_syntactic_all.png"
scp ${host}:${base_folder}/aug_STRONG_full/graphs/coverage${threshould}.png      "${out_path}/cov_full_strong.png"
scp ${host}:${base_folder}/aug_STRONG_lexical/graphs/coverage${threshould}.png   "${out_path}/cov_lexical_strong.png"
scp ${host}:${base_folder}/aug_STRONG_semantic/graphs/coverage${threshould}.png  "${out_path}/cov_semantic_strong.png"
scp ${host}:${base_folder}/aug_STRONG_syntactic/graphs/coverage${threshould}.png "${out_path}/cov_syntactic_strong.png"
scp ${host}:${base_folder}/aug_WEAK_full/graphs/coverage${threshould}.png        "${out_path}/cov_full_weak.png"
scp ${host}:${base_folder}/aug_WEAK_lexical/graphs/coverage${threshould}.png     "${out_path}/cov_lexical_weak.png"
scp ${host}:${base_folder}/aug_WEAK_semantic/graphs/coverage${threshould}.png    "${out_path}/cov_semantic_weak.png"
scp ${host}:${base_folder}/aug_WEAK_syntactic/graphs/coverage${threshould}.png   "${out_path}/cov_syntactic_weak.png"

