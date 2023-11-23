# _targets_pre_post.R #

# Source the functions that will be used to build the targets in _targets_pre_post.R


# Targets:
list(
  
  # Load ema_data created by another targets list.
  tar_load(ema_data),
  
  # Get estimate and effect size of negative affect pre-exam - post-exam
  # First exam
  tar_target(
    list_params_first_exam_neg_aff_difference,
    compute_exam_effects_on_neg_aff(ema_data, "first_exam")
  ),

  # Get estimate and effect size of negative affect pre-exam - post-exam
  # Second exam
  tar_target(
    list_params_second_exam_neg_aff_difference,
    compute_exam_effects_on_neg_aff(ema_data, "second_exam")
  ),

  # Comparison of the average negative affect in the days not coinciding with
  # the exam day (pre, post) and either pre (the day before the exam) or post
  # (the evening after the exam). Here is the comparison between the average
  # negative affect in the no-exam days and the day before the exam.
  # Positive values of the beta coefficient mean that the tested condition
  # pre or post has a larger value than the comparison condition (always the
  # average negative affect in the no-exam days). Positive values of beta, in
  # the pre condition mean that the day before the exam the negative affect was
  # more intense than in the previous (or following) days.
  tar_target(
    list_params_no_exam_pre_neg_aff_difference,
    compute_no_exam_effects_on_neg_aff(
      gen_data_comparison_avg_pre_post_neg_aff(ema_data, "pre"), 
      "pre"
    )
  ),

  # Here is the comparison between the average negative affect in the no-exam
  # days and the evening after the exam.
  tar_target(
    list_params_no_exam_post_neg_aff_difference,
    compute_no_exam_effects_on_neg_aff(
      gen_data_comparison_avg_pre_post_neg_aff(ema_data, "post"), 
      "post"
    )
  )
  
  # close list  
)


