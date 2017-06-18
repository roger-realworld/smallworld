#% text_encoding = iso8859_1
### Internationalisation of Smallworld Emacs.
+ emacs_patch

:|patch-menu|                               :en_gb Patch

:|patch-fields-readonly|                    :en_gb Magik Patch fields are now Read-only.
:|patch-fields-not-readonly|                :en_gb Magik Patch fields are no longer Read-only - you're on your own!
:|patch-field-already-complete|             :en_gb '#1' is already a valid entry for #2.
:|patch-empty-string-is-valid|              :en_gb Note, empty string is a valid entry for #1.
:|patch-bug-number-prompt|                  :en_gb Bug number:
:|patch-rename-prompt|                      :en_gb Rename Patch #1 to:
:|patch-select-directory-prompt|            :en_gb Select Directory [use <ret> or <tab> for completion]:
:|patch-warn-separator-lines|               :en_gb Found #1 separator lines. Should be 1 or 2
:|patch-queried|                            :en_gb Patch '#1' queried.
:|patch-reviewed|                           :en_gb Patch '#1' reviewed
:|patch-processing|                         :en_gb Processing #1...
:|patch-processing-done|                    :en_gb Processing done
:|patch-creating-caches|                    :en_gb Creating the patch caches (#1)...
:|patch-creating-bug-cache|                 :en_gb Creating a bug cache (this will take a few seconds)...
:|patch-load-topic-data|                    :en_gb Loading topic data (this will take a few seconds)...
:|patch-view-rejected-patch|		    :en_gb '#1' is a rejected patch. Entered View Mode.
:|patch-avoid-rejected-patch|		    :en_gb '#1' is a rejected patch. To view, use C-u M-x #2

:|patch-help-convert|                       :en_gb Use #1 to Convert to a different template type
:|patch-help-rename|                        :en_gb Use #1 to Rename the patch number
:|patch-help-resurrected|                   :en_gb Patch resurrected: please check, fill in target release and resubmit

:|patch-yn-delete-original|                 :en_gb Delete original patch file #1?
:|patch-yn-enable-minor-mode|               :en_gb Magik buffer is not in the Patch minor mode.
:|patch-yn-save-changes|                    :en_gb Buffer modified - save changes?
:|patch-yn-file-save-changes|               :en_gb Buffer for file #1 modified - save changes?
:|patch-yn-confirm-approval|                :en_gb Confirm approval modifications:
:|patch-yn-overwrite|                       :en_gb Overwrite #1?
:|patch-yn-save-unsubmitted|                :en_gb Save changes to unsubmitted patches directory?

:|patch-not-a-patch-file-error|             :en_gb Not a Magik Patch file
:|patch-number-error|                       :en_gb #1 is not a valid patch pattern: should be N_n[a] where 100 < N < 99999999, n > 1 and a is a lowercase letter
:|patch-already-exists-error|               :en_gb Patch #1 either already exists or it is an invalid name to choose
:|patch-invalid-major-mode-error|           :en_gb Magik Patch mode is not defined for #1 mode
:|patch-not-in-minor-mode-error|            :en_gb Not in Magik Patch minor mode
:|patch-field-value-invalid|                :en_gb Current field, #1, value '#2' is not valid
:|patch-type-key-unknown|                   :en_gb Unknown #1 '#2'
:|patch-no-field-error|                     :en_gb Cannot find field '#1'
:|patch-field-no-completions-error|         :en_gb No #1 completions of #2
:|patch-no-topic-for-stream-error|          :en_gb No #1 found for '#2'
:|patch-no-subtopic-for-topic-stream-error| :en_gb No #1 found for '#2' '#3'
:|patch-no-developer-error|                 :en_gb No #1 found for '#2' '#3' '#4'
:|patch-no-reject-error|                    :en_gb Rejected file not found: #1
:|patch-no-file-error|                      :en_gb File not available: #1
:|patch-invalid-template-type-error|        :en_gb Cannot use template file type #1 in Magik Patch minor mode
:|patch-proposed-already-error|             :en_gb A Patch already exists in the proposed area with this name
:|patch-submitted-already-error|            :en_gb Patch #1 already submitted
:|patch-no-change-number-field-error|       :en_gb Cannot locate a field where the change number is #1
:|patch-not-submitted-error|                :en_gb #1 not submitted!
:|patch-not-approved-error|                 :en_gb #1 not approved!
:|patch-name-file-mismatch-error|           :en_gb #1 number #2 does not match file name P#1.magik
:|patch-no-author|                          :en_gb No author!
:|patch-fill-in-field-error|                :en_gb Please fill in the '#1' field.
:|patch-fill-in-mandatory-field-error|      :en_gb Please fill in the Mandatory '#1' field.
:|patch-not-fill-in-field-error|            :en_gb Please do NOT fill in the '#1' field.
:|patch-software-line-invalid-error|        :en_gb sw!patch_software line contains < > characters. Not filled in?
:|patch-requires-line-invalid-error|        :en_gb sw!patch_requires line contains < > characters. Not filled in?
:|patch-requires-mismatch-error|            :en_gb sw!patch_requires line does not match patch number '#1'
:|patch-declare-mismatch-error|             :en_gb sw!declare_patch line does not match patch number '#1'
:|patch-declare-invalid-error|              :en_gb Please fill in the '#1' description.  #2 not submitted
:|patch-fill-in-field-approve-error|        :en_gb Patch #1 is not approved. #2 field is not filled in
:|patch-not-edited-error|                   :en_gb Patch #1 has never been edited!
:|patch-already-approved-error|             :en_gb Patch #1 already approved in stream #2
:|patch-already-in-unsubmitted-error|       :en_gb File already exists in unsubmitted patches directory
:|patch-missing-required-file-error|        :en_gb Required file, '#1', does not exist in stream, #2
:|patch-invalid-primary-stream-error|       :en_gb The '#1' field primary stream value '#2' is not valid
:|patch-invalid-secondary-stream-error|     :en_gb The '#1' field secondary stream value in '#2' is not valid
:|patch-invalid-release-version-error|      :en_gb Intended Release version, '#1', not known.  Nothing submitted
:|patch-abandoned|                          :en_gb abandoned!
:|patch-action-start|                       :en_gb Performing #1 Magik Patch Process in buffer #2...
:|patch-action-complete|                    :en_gb Performing #1 Magik Patch Process in buffer #2...done
:|patch-action-failed|                      :en_gb Performing #1 Magik Patch Process in buffer #2...failed
:|patch-action-no-command-error|            :en_gb Action '#1' has no command defined to run
:|patch-action-no-value-error|              :en_gb Action '#1' has been given an invalid value for argument '#2'
