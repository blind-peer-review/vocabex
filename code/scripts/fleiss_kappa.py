import sys
from vocabex.fleiss_kappa import fleiss_kappa_report

# k for exercise_type
output_file = open("./outputs/fleiss_kappa_error_type_results.txt", "w")
sys.stdout = output_file
fleiss_kappa_report("./resources/fleiss_kappa_error_type.csv", 'A01', 'A02', 'A03')
