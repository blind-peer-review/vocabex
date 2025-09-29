import sys
from vocabex.cohen_kappa import cohen_kappa_report

# k for exercise_type
output_file = open("./outputs/cohen_kappa_exercise_type_results.txt", "w")
sys.stdout = output_file
cohen_kappa_report("./resources/cohen_kappa_exercise_type.csv", 'rater_1', 'rater_2')

# k for exercise_format
output_file = open("./outputs/cohen_kappa_exercise_format_results.txt", "w")
sys.stdout = output_file
cohen_kappa_report("./resources/cohen_kappa_exercise_format.csv", 'rater_1', 'rater_2')
