;;; Package --- Messages for Smallworld.

;;; Commentary:

;;; Code:

(setq resources-aliases-menu
      "Aliases")
(setq resources-aliases-menu-run-gis
      "Run Current Definition")
(setq resources-aliases-menu-definitions
      "Definitions")
(setq resources-aliases-menu-none-found
      "No Aliases found")
(setq resources-aliases-sw-menu-rescan
      "*Rescan*")
(setq resources-aliases-definition-prompt
      "Definition:")
(setq resources-aliases-cwd-prompt
      "Set current working directory:")
(setq resources-aliases-command
      "Command")
(setq resources-aliases-cwd
      "Cwd is: %s")
(setq resources-aliases-no-definition
      "Cannot find any alias definitions")
(setq resources-menu-sw-cb
      "Class Browser")
(setq resources-cb-menu-start
      "Run/Goto Class Browser")
(setq resources-cb-menu-start-new-cb
      "Start New Class Browser")
(setq resources-cb-menu-start-method
      "Paste Method in CB")
(setq resources-cb-menu-start-class
      "Paste Class in CB")
(setq resources-cb-menu-start-m-c
      "Clear Method and Class in CB")
(setq resources-cb-menu
      "CB")
(setq resources-cb-menu-jump
      "Jump to Source")
(setq resources-cb-menu-family-tree
      "Family Tree")
(setq resources-cb-menu-fold
      "Fold")
(setq resources-cb-menu-unfold
      "Unfold")
(setq resources-cb-menu-options
      "Set Options")
(setq resources-cb-menu-toggle-topics
      "Turn All Topics On/Off")
(setq resources-cb-menu-reset
      "Reset All Options")
(setq resources-cb-menu-toggle-override-flags
      "Override Flags")
(setq resources-cb-menu-toggle-override-topics
      "Override Topics")
(setq resources-cb-menu-toggle-override-200-limit
      "Override 200 Limit")
(setq resources-cb-menu-tab
      "Hop")
(setq resources-cb-menu-clear
      "Clear")
(setq resources-cb-menu-clear-m-c
      "Clear Method and Class")
(setq resources-cb-menu-quit
      "Hide")
(setq resources-cb-menu-gis
      "Gis Process")
(setq resources-cb-menu-gis-shell
      "Gis Shell Process")
(setq resources-cb-in
      "IN")
(setq resources-cb-enter-buffer
      "Enter Class Browser buffer:")
(setq resources-cb-or-gis-enter-buffer
      "Enter Class Browser or Gis process buffer:")
(setq resources-cb-enter-load-file
      "Method Finder File:")
(setq resources-cb-enter-yn-load-file
      "`%s' doesn't seem to be a method_finder file.  Load anyway?")
(setq resources-cb-loading
      "Loading the documentation ...")
(setq resources-cb-inherit-local
      "Setting inheritance mode to 'local-only'.")
(setq resources-cb-inherit-object
      "Setting inheritance mode to 'inherit-from-\"object\"'.")
(setq resources-cb-inherit-not-object
      "Setting inheritance mode to 'inherit-not-\"object\"'.")
(setq resources-cb-gis-get-mf-busy
      "The GIS process in buffer %s is busy... Please wait for CB to start")
(setq resources-cb-gis-get-mf-abort
      "The CB cannot start yet because the GIS process in %s is busy... Abort CB")
(setq resources-cb-goto-beginning
      "Press `C-a' again to go right to the beginning.")
(setq resources-cb-goto-end
      "Press `C-e' again to go right to the end.")
(setq resources-cb-yank-one-line
      "Only yanked the first line.")
(setq resources-cb-toggle-flag-on
      "Turning '%s' flag on.")
(setq resources-cb-toggle-flag-off
      "Turning '%s' flag off.")
(setq resources-cb-fully-unfolded
      "The methods are now fully unfolded")
(setq resources-cb-fully-folded
      "The methods are now fully folded")
(setq resources-cb-modeline-help-flag
      "mouse-1, mouse-2: toggle %s flag")
(setq resources-cb-modeline-help-buffer
      "mouse-1, mouse-2: Switch to buffer %s")
(setq resources-cb-modeline-help-cursor
      "TAB: switch between class and method name")
(setq resources-cb-file-is-dir-error
      "Please give a filename of an mf file")
(setq resources-cb-file-not-loaded
      "%s not loaded")
(setq resources-cb-getenv-error
      "There is no value for the environment variable, '%s'")
(setq resources-cb-cannot-jump-error
      "cb-goto-method (can't jump): %s")
(setq resources-cb-no-code-error
      "There is no source code for '%s.%s'")
(setq resources-cb-no-file-error
      "Cannot find file, %s")
(setq resources-cb-no-in-line-error
      "Can't find a line like: 'my_method  IN  my_class'")
(setq resources-cb-not-running-error
      "The Class Browser, '%s', is not running")
(setq resources-cb-none-running-error
      "No Class Browser is running")
(setq resources-cb-no-current-word-error
      "No current word to use as a method name")
(setq resources-cb-mf-version-error
      "unknown - using call-process on the method_finder failed")
(setq resources-cb-already-unfolded-error
      "The methods are already fully unfolded")
(setq resources-cb-already-folded-error
      "The methods are already fully folded")
(setq resources-cb-no-current-class-error
      "No current word to use as a class-name")
(setq resources-deep-print-menu
      "Deep_print")
(setq resources-deep-print-menu-unfold
      "Unfold")
(setq resources-deep-print-menu-start
      "Deep Print")
(setq resources-deep-print-prompt
      "Explore the magik value")
(setq resources-deep-print-unfold-limit-error
      "Can't expand this line.")
(setq resources-dev-tools-menu-vsd-method
      "Dev Tools - Debug Method")
(setq resources-dev-tools-menu-oi
      "Dev Tools - Object Inspector")
(setq resources-dev-tools-menu-tb
      "Dev Tools - Traceback Viewer")
(setq resources-g-v-menu
      "Environment")
(setq resources-g-v-menu-select
      "Select")
(setq resources-g-v-menu-run
      "Run Gis Command")
(setq resources-g-v-menu-next
      "Next Environment")
(setq resources-g-v-menu-aliases
      "Open Aliases File")
(setq resources-g-v-menu-quit
      "Quit")
(setq resources-g-v-menu-file-add
      "Add New Installation")
(setq resources-g-v-menu-reset
      "Reset Process Environment")
(setq resources-g-v-yn-file-create
      "Create File Interface for SW Environments?")
(setq resources-g-v-yn-reset
      "Are you sure you want to reset the process environment?")
(setq resources-g-v-file-prompt
      "Enter New Environment List File:")
(setq resources-g-v-smallworld-gis-prompt
      "Enter product directory for Core installation:")
(setq resources-g-v-name-prompt
      "Enter name for this installation:")
(setq resources-g-v-version-prompt
      "Enter version number of this installation:")
(setq resources-g-v-lp-alias-prompt
      "Select a Layered Product with gis_aliases file:")
(setq resources-g-v-starting-command
      "Starting %s selection...")
(setq resources-g-v-starting-command-done
      "Starting %s selection... done")
(setq resources-g-v-current
      "The current installation for this Emacs is now %s.")
(setq resources-g-v-current-version
      "Gis Environment: %s")
(setq resources-g-v-invalid-warning-1
      "** Can't find the currently selected product, %s.")
(setq resources-g-v-invalid-warning-2
      "** (Attempting to run anyway).")
(setq resources-g-v-no-command-error
      "Cannot find the %s command")
(setq resources-g-v-position-error
      "No Environment at this point")
(setq resources-g-v-invalid-error
      "You have selected an (invalid) Environment")
(setq resources-g-v-match-error
      "No Environment on this line")
(setq resources-g-v-invalid-selection-error
      "Invalid selection")
(setq resources-g-v-file-not-used-error
      "File interface is not being used")
(setq resources-gis-menu
      "Gis")
(setq resources-gis-menu-previous
      "Previous Command")
(setq resources-gis-menu-next
      "Next Command")
(setq resources-gis-menu-previous-match
      "Previous Matching Command")
(setq resources-gis-menu-next-match
      "Next Matching Command")
(setq resources-gis-menu-fold
      "Fold")
(setq resources-gis-menu-unfold
      "Unfold")
(setq resources-gis-menu-magik-template
      "Electric Template")
(setq resources-gis-menu-complete
      "Symbol Complete")
(setq resources-gis-menu-tb-previous
      "Previous Traceback")
(setq resources-gis-menu-tb-next
      "Next Traceback")
(setq resources-gis-menu-tb-print
      "Print Traceback")
(setq resources-gis-menu-tb-save
      "Save Traceback")
(setq resources-gis-menu-shell
      "Gis Shell Process")
(setq resources-gis-menu-kill
      "Kill Gis Process")
(setq resources-gis-menu-history
      "Gis Command History")
(setq resources-gis-menu-no-history
      "No History")
(setq resources-gis-menu-toggle
      "Toggle...")
(setq resources-gis-menu-toggle-filter
      "GIS Process Filter")
(setq resources-gis-menu-toggle-drag-n-drop
      "Drag and Drop")
(setq resources-gis-command-prompt
      "Gis command:")
(setq resources-gis-enter-buffer
      "Enter Gis process buffer:")
(setq resources-gis-resizing-vector
      "Re-sizing the command history vector...")
(setq resources-gis-resizing-vector-done
      "Re-sizing the command history vector...Done. (%s commands).")
(setq resources-gis-not-sent-waiting-for
      "Not sent (waiting for '%s').")
(setq resources-gis-not-send-pending-op
      "Not sent (there is a pending operator '%s').")
(setq resources-gis-folding-commands
      "Folding the last %s commands...")
(setq resources-gis-folding-commands-done
      "Folding the last %s commands...Done")
(setq resources-gis-unfolding-commands
      "Unfolding the last %s commands...")
(setq resources-gis-unfolding-commands-done
      "Unfolding the last %s commands...Done")
(setq resources-gis-printing
      "Printing...")
(setq resources-gis-printing-done
      "Printing...Done")
(setq resources-gis-save-traceback
      "Saved the traceback in '%s'.")
(setq resources-gis-kill-process-pending
      "Magik is still busy and will exit at an appropriate point. Please be patient...")
(setq resources-gis-sentinel-process-text
      "Process %s %s")
(setq resources-gis-sentinel-process-exited
      "GIS %s exited: %s")
(setq resources-gis-sentinel-process-signalled
      "GIS %s signalled: %s")
(setq resources-gis-sentinel-process-terminated
      "GIS %s process %s has terminated with exit code: %s")
(setq resources-gis-drag-n-drop-on
      "GIS Drag 'n' Drop file mode is on")
(setq resources-gis-drag-n-drop-off
      "GIS Drag 'n' Drop file mode is off")
(setq resources-gis-filtering-magik
      "Filtering Magik output...(%s chars)")
(setq resources-gis-filter-set
      "Set the filter in '%s'.")
(setq resources-gis-filter-cancelled
      "Cancelled the filter in '%s'.")
(setq resources-gis-filter-char-set
      "Filter action for '%s' is already set")
(setq resources-gis-filter-complete-no-match
      "Cannot find completion for %s.")
(setq resources-gis-filter-complete-sole
      "Sole completion.")
(setq resources-gis-filter-complete-done
      "Finding completions...Done.")
(setq resources-gis-yn-kill-gis
      "Kill the Gis?")
(setq resources-gis-yn-suspend-gis
      "Suspend the Gis?")
(setq resources-gis-yn-eof-gis
      "Send EOF to the Gis?")
(setq resources-gis-yn-kill-region
      "Cutting and pasting big regions can confuse the gis-mode markers. Kill anyway?")
(setq resources-gis-yn-print-last-traceback
      "Print the last traceback (%s lines)?")
(setq resources-gis-scan-error
      "%s or quotes")
(setq resources-gis-long-line-error
      "Sending long lines will probably crash the gis buffer. Use load_file instead.")
(setq resources-gis-no-end-token-error
      "Found '%s' with no corresponding '%s'")
(setq resources-gis-expecting-token-error
      "Found '%s' when expecting '%s'")
(setq resources-gis-command-recall-error
      "Sorry... Confused command recall")
(setq resources-gis-no-process-error
      "There is no process running in this buffer")
(setq resources-gis-not-command-error
      "Not a command")
(setq resources-gis-no-previous-command-error
      "No previous command")
(setq resources-gis-no-previous-matching-command-error
      "No previous command matching '%s'")
(setq resources-gis-no-commands-to-fold
      "No commands to fold")
(setq resources-gis-no-commands-to-unfold
      "No commands to unfold")
(setq resources-gis-no-error-line-print-error
      "Couldn't find a line starting with '%s' - nothing printed")
(setq resources-gis-no-error-line-save-error
      "Couldn't find a line starting with '%s' - nothing saved")
(setq resources-gis-filter-char-error
      "No filter defined for character %s")
(setq resources-gis-goto-error-invalid-line
      "No Error on this line to go to")
(setq resources-auto-gis-load-file
      "Gis process buffer %s : Loading file %s")
(setq resources-auto-gis-no-command-error
      "No Gis command given on command line")
(setq resources-gis-version-help
      "Select a Smallworld Core Product Installation.

The product you select will define the environment for any new Smallworld
sessions that `F2 z' runs.

To make the selection, move the cursor to the line you want and press RETURN.

Press q to exit and do nothing.
")
(setq resources-gis-version-help-file-add
      "To add a new installation, press +.
")
(setq resources-gis-version-none-selected
      "** There is no currently selected gis product.
** (Attempting to run anyway).
")
(setq resources-cb-header
      "    Smallworld Class Browser (Version 2.0)
    --------------------------------------
")
(setq resources-cb-gis-no-mf-file
      "*** Can't start the Class Browser. ***

  The gis hasn't started a method_finder.
  Perhaps there was no '.mf' file next to your image file.
")
(setq resources-cb-topic-header
      "    CLASS BROWSER CONTROL PANEL : up to 200 methods normally displayed

    PRAGMA FLAGS        INHERITANCE                    LAYOUT
")
(setq resources-cb-topic-overrides
      "    OVERRIDES : use these for temporary changes to the control panel
")
(setq resources-cb-topic-toggles
      "    TOPICS : use F3 t to turn all topics on/off
")
(setq resources-mpde-available-configurations-help
      "* marks currently selected configuration
x indicates configuration not available or misconfigured
")
(setq resources-pragma-topic-selection
      "T O P I C   S E L E C T I O N

y or m - mark a line           n or u - unmark a line
e      - edit the topic list
SPC    - move down a line
RET    - accept the selection
q      - quit

-----------------------------------------------
")
(setq resources-loadlist-menu
      "Loadlist")
(setq resources-loadlist-menu-refresh
      "Refresh Buffer from Directory")
(setq resources-loadlist-menu-transmit
      "Transmit Buffer")
(setq resources-loadlist-menu-toggle-compile
      "Toggle: Save Compiled Files")
(setq resources-loadlist-compile-files-on
      "Will compile files when loading.")
(setq resources-loadlist-compile-files-off
      "Will not (re)compile files when loading.")
(setq resources-loadlist-loaded-in-buffer
      "%s loaded in buffer %s.")
(setq resources-loadlist-output-updated
      "Updated '%s' with '%s'")
(setq resources-loadlist-output-append
      "Append '%s'")
(setq resources-loadlist-output-ignored
      "Ignored '%s'")
(setq resources-loadlist-output-remove
      "Remove '%s'")
(setq resources-loadlist-output-no-changes
      "No changes required in buffer")
(setq resources-patch-menu
      "Patch")
(setq resources-patch-fields-readonly
      "Magik Patch fields are now Read-only.")
(setq resources-patch-fields-not-readonly
      "Magik Patch fields are no longer Read-only - you're on your own!")
(setq resources-patch-field-already-complete
      "'%s' is already a valid entry for %s.")
(setq resources-patch-empty-string-is-valid
      "Note, empty string is a valid entry for %s.")
(setq resources-patch-bug-number-prompt
      "Bug number:")
(setq resources-patch-rename-prompt
      "Rename Patch %s to:")
(setq resources-patch-select-directory-prompt
      "Select Directory [use <ret> or <tab> for completion]:")
(setq resources-patch-warn-separator-lines
      "Found %s separator lines. Should be 1 or 2")
(setq resources-patch-queried
      "Patch '%s' queried.")
(setq resources-patch-reviewed
      "Patch '%s' reviewed")
(setq resources-patch-processing
      "Processing %s...")
(setq resources-patch-processing-done
      "Processing done")
(setq resources-patch-creating-caches
      "Creating the patch caches (%s)...")
(setq resources-patch-creating-bug-cache
      "Creating a bug cache (this will take a few seconds)...")
(setq resources-patch-load-topic-data
      "Loading topic data (this will take a few seconds)...")
(setq resources-patch-view-rejected-patch
      "'%s' is a rejected patch. Entered View Mode.")
(setq resources-patch-avoid-rejected-patch
      "'%s' is a rejected patch. To view, use C-u M-x %s")
(setq resources-patch-help-convert
      "Use %s to Convert to a different template type")
(setq resources-patch-help-rename
      "Use %s to Rename the patch number")
(setq resources-patch-help-resurrected
      "Patch resurrected: please check, fill in target release and resubmit")
(setq resources-patch-yn-delete-original
      "Delete original patch file %s?")
(setq resources-patch-yn-enable-minor-mode
      "Magik buffer is not in the Patch minor mode.")
(setq resources-patch-yn-save-changes
      "Buffer modified - save changes?")
(setq resources-patch-yn-file-save-changes
      "Buffer for file %s modified - save changes?")
(setq resources-patch-yn-confirm-approval
      "Confirm approval modifications:")
(setq resources-patch-yn-overwrite
      "Overwrite %s?")
(setq resources-patch-yn-save-unsubmitted
      "Save changes to unsubmitted patches directory?")
(setq resources-patch-not-a-patch-file-error
      "Not a Magik Patch file")
(setq resources-patch-number-error
      "%s is not a valid patch pattern: should be N_n[a] where 100 < N < 99999999, n > 1 and a is a lowercase letter")
(setq resources-patch-already-exists-error
      "Patch %s either already exists or it is an invalid name to choose")
(setq resources-patch-invalid-major-mode-error
      "Magik Patch mode is not defined for %s mode")
(setq resources-patch-not-in-minor-mode-error
      "Not in Magik Patch minor mode")
(setq resources-patch-field-value-invalid
      "Current field, %s, value '%s' is not valid")
(setq resources-patch-type-key-unknown
      "Unknown %s '%s'")
(setq resources-patch-no-field-error
      "Cannot find field '%s'")
(setq resources-patch-field-no-completions-error
      "No %s completions of %s")
(setq resources-patch-no-topic-for-stream-error
      "No %s found for '%s'")
(setq resources-patch-no-subtopic-for-topic-stream-error
      "No %s found for '%s' '%s'")
(setq resources-patch-no-developer-error
      "No %s found for '%s' '%s' '%s'")
(setq resources-patch-no-reject-error
      "Rejected file not found: %s")
(setq resources-patch-no-file-error
      "File not available: %s")
(setq resources-patch-invalid-template-type-error
      "Cannot use template file type %s in Magik Patch minor mode")
(setq resources-patch-proposed-already-error
      "A Patch already exists in the proposed area with this name")
(setq resources-patch-submitted-already-error
      "Patch %s already submitted")
(setq resources-patch-no-change-number-field-error
      "Cannot locate a field where the change number is %s")
(setq resources-patch-not-submitted-error
      "%s not submitted!")
(setq resources-patch-not-approved-error
      "%s not approved!")
(setq resources-patch-name-file-mismatch-error
      "%s number %s does not match file name P%s.magik")
(setq resources-patch-no-author
      "No author!")
(setq resources-patch-fill-in-field-error
      "Please fill in the '%s' field.")
(setq resources-patch-fill-in-mandatory-field-error
      "Please fill in the Mandatory '%s' field.")
(setq resources-patch-not-fill-in-field-error
      "Please do NOT fill in the '%s' field.")
(setq resources-patch-software-line-invalid-error
      "sw!patch_software line contains < > characters. Not filled in?")
(setq resources-patch-requires-line-invalid-error
      "sw!patch_requires line contains < > characters. Not filled in?")
(setq resources-patch-requires-mismatch-error
      "sw!patch_requires line does not match patch number '%s'")
(setq resources-patch-declare-mismatch-error
      "sw!declare_patch line does not match patch number '%s'")
(setq resources-patch-declare-invalid-error
      "Please fill in the '%s' description.  %s not submitted")
(setq resources-patch-fill-in-field-approve-error
      "Patch %s is not approved. %s field is not filled in")
(setq resources-patch-not-edited-error
      "Patch %s has never been edited!")
(setq resources-patch-already-approved-error
      "Patch %s already approved in stream %s")
(setq resources-patch-already-in-unsubmitted-error
      "File already exists in unsubmitted patches directory")
(setq resources-patch-missing-required-file-error
      "Required file, '%s', does not exist in stream, %s")
(setq resources-patch-invalid-primary-stream-error
      "The '%s' field primary stream value '%s' is not valid")
(setq resources-patch-invalid-secondary-stream-error
      "The '%s' field secondary stream value in '%s' is not valid")
(setq resources-patch-invalid-release-version-error
      "Intended Release version, '%s', not known.  Nothing submitted")
(setq resources-patch-abandoned
      "abandoned!")
(setq resources-patch-action-start
      "Performing %s Magik Patch Process in buffer %s...")
(setq resources-patch-action-complete
      "Performing %s Magik Patch Process in buffer %s...done")
(setq resources-patch-action-failed
      "Performing %s Magik Patch Process in buffer %s...failed")
(setq resources-patch-action-no-command-error
      "Action '%s' has no command defined to run")
(setq resources-patch-action-no-value-error
      "Action '%s' has been given an invalid value for argument '%s'")
(setq resources-magik-menu
      "Magik")
(setq resources-magik-menu-transmit-method
      "Transmit Method")
(setq resources-magik-menu-transmit-region
      "Transmit Region")
(setq resources-magik-menu-transmit-buffer
      "Transmit Buffer")
(setq resources-magik-menu-transmit-chunk
      "Transmit $ Chunk")
(setq resources-magik-menu-transmit-thing
      "Transmit Thing")
(setq resources-magik-menu-work-copy-method
      "Copy Method to Work Buffer")
(setq resources-magik-menu-work-copy-region
      "Copy Region to Work Buffer")
(setq resources-magik-menu-work-set-buffer
      "Set Work Buffer Name")
(setq resources-magik-menu-electric-template
      "Electric Template")
(setq resources-magik-menu-mark-method
      "Mark Method")
(setq resources-magik-menu-copy-method
      "Copy Method")
(setq resources-magik-menu-compare-methods
      "Compare Method between Windows")
(setq resources-magik-menu-ediff-methods
      "Compare Method using Ediff")
(setq resources-magik-menu-cb-ediff-methods
      "Compare Method using CB and Ediff")
(setq resources-magik-menu-add-debug
      "Add Debug Statement")
(setq resources-magik-menu-trace
      "Trace Statement")
(setq resources-magik-menu-complete
      "Symbol Complete")
(setq resources-magik-menu-heading
      "Heading")
(setq resources-magik-menu-comment
      "Comment Region")
(setq resources-magik-menu-uncomment
      "Uncomment Region")
(setq resources-magik-menu-fill
      "Fill Comment")
(setq resources-magik-menu-toggle
      "Toggle...")
(setq resources-magik-menu-toggle-name-mode
      "Method Name Display")
(setq resources-magik-menu-toggle-electric-mode
      "Electric Magik Mode")
(setq resources-magik-menu-toggle-debug
      "Enable #Debug Statements")
(setq resources-magik-menu-toggle-mark-exchange
      "Point at End of Marked Region")
(setq resources-magik-menu-options
      "Options:")
(setq resources-magik-menu-option-eom-mode-on
      "Transmit Method = Move to End")
(setq resources-magik-menu-option-eom-mode-off
      "Transmit Method = Do Not Move Point")
(setq resources-magik-menu-option-eom-mode-repeat
      "Transmit Method = On Repeat, Move to End")
(setq resources-magik-menu-language-help
      "Help: Magik Language Reference")
(setq resources-magik-imenu-package
      "Package")
(setq resources-magik-imenu-globals
      "Globals")
(setq resources-magik-imenu-exemplars
      "Exemplars")
(setq resources-magik-imenu-arrays
      "Arrays")
(setq resources-magik-imenu-binary-operator
      "Operators")
(setq resources-magik-imenu-mixins
      "Mixins")
(setq resources-magik-imenu-pseudo-slots
      "Pseudo Slots")
(setq resources-magik-imenu-slot-access
      "Slot Access")
(setq resources-magik-imenu-shared-constants
      "Shared Constants")
(setq resources-magik-imenu-shared-variables
      "Shared Variables")
(setq resources-magik-imenu-property
      "Properties")
(setq resources-magik-imenu-conditions
      "Conditions")
(setq resources-magik-imenu-procedures
      "Procedures")
(setq resources-magik-imenu-new
      "new/init")
(setq resources-magik-imenu-display
      "show/write/print/trace")
(setq resources-magik-imenu-abstract
      "Abstract")
(setq resources-magik-imenu-private
      "Private")
(setq resources-magik-imenu-iterators
      "Iterators")
(setq resources-magik-imenu-methods
      "Public Methods")
(setq resources-magik-electric-template-prompt
      "Electric template:")
(setq resources-magik-goto-method-prompt
      "Method Name")
(setq resources-magik-goto-class-prompt
      "Class Name")
(setq resources-magik-goto-class-method-loop-start-prompt
      "Warning: Goto first definition of '%s'")
(setq resources-magik-goto-class-method-loop-prompt
      "Warning: Goto next definition of '%s'")
(setq resources-magik-electric-on
      "Electric Magik on.")
(setq resources-magik-electric-off
      "Electric Magik off.")
(setq resources-magik-method-name-on
      "Method name display on.")
(setq resources-magik-method-name-off
      "Method name display off.")
(setq resources-transmit-magik-debug-on
      "Magik DEBUG statements on")
(setq resources-transmit-magik-debug-off
      "Magik DEBUG statements off")
(setq resources-magik-mark-method-exchange-on
      "Cursor will be placed at end of marked region.")
(setq resources-magik-mark-method-exchange-off
      "Cursor will be placed at start of marked region.")
(setq resources-magik-transmit-method-eom-on
      "After transmit method cursor will move to end of method")
(setq resources-magik-transmit-method-eom-off
      "After transmit method the cursor position will be unaffected")
(setq resources-magik-transmit-method-eom-on-repeat
      "After transmit method, cursor will move to end of method when command is repeated.")
(setq resources-magik-transmit-string
      "Transmitting to %s")
(setq resources-magik-code-loaded
      "Code loaded from %s")
(setq resources-magik-complete-empty-string
      "Doing a completion on the empty string would take too long")
(setq resources-magik-complete-string
      "Symbol is already complete or is too short.")
(setq resources-magik-mode-is-gis-error
      "Your gis buffer has got into magik mode!  To recover, type `M-x gis-mode'.  Please report this bug.")
(setq resources-magik-expecting-token-error
      "Found '%s' when expecting '%s'")
(setq resources-magik-transmit-error
      "Don't know what to transmit")
(setq resources-magik-no-round-bracket-error
      "Can't find closing round ) bracket")
(setq resources-magik-no-square-bracket-error
      "Can't find closing square ] bracket")
(setq resources-magik-unquoted-multibyte-error
      "Found unquoted multibyte character at position %s")
(setq resources-magik-electric-no-template-error
      "There is no template for %s")
(setq resources-magik-no-current-variable-error
      "No current variable to print")
(setq resources-magik-goto-no-m-error
      "Cannot find method '%s'")
(setq resources-magik-goto-no-m-in-c-error
      "Cannot find method, '%s', in class, '%s'")
(setq resources-menu-sw-dev
      "Dev")
(setq resources-menu-sw-dev-get-bug
      "Get Bug Report")
(setq resources-menu-sw-dev-make-patch
      "Get/make Patch")
(setq resources-menu-sw-dev-make-cn
      "Make Change Note")
(setq resources-menu-sw-dev-make-unit-test
      "Make Unit Test")
(setq resources-menu-sw-dev-submit
      "Submit Patch")
(setq resources-menu-sw-dev-review
      "Review Patch")
(setq resources-menu-sw-dev-query
      "Query Patch")
(setq resources-menu-sw-dev-approve
      "Approve Patch")
(setq resources-menu-sw-dev-unsubmit
      "Unsubmit Patch")
(setq resources-menu-sw-dev-unreview
      "UnReview Patch")
(setq resources-menu-sw-dev-reject
      "Reject Patch")
(setq resources-menu-sw-dev-resurrect
      "Resurrect Patch")
(setq resources-menu-sw-dev-help
      "Patch Help")
(setq resources-menu-sw
      "SW")
(setq resources-menu-sw-gis-select
      "Select Smallworld Environment")
(setq resources-menu-sw-run-gis
      "Run/Goto Smallworld Session")
(setq resources-menu-sw-run-new-gis
      "Start New Smallworld Session")
(setq resources-menu-sw-alias-files
      "Alias Files")
(setq resources-menu-sw-gis-procs
      "Gis Processes")
(setq resources-menu-sw-cb-procs
      "Class Browser Processes")
(setq resources-menu-sw-shell-procs
      "Shell Processes")
(setq resources-menu-sw-list-procs
      "List Processes")
(setq resources-menu-sw-no-procs
      "No Processes")
(setq resources-menu-sw-tools
      "Tools")
(setq resources-menu-sw-help
      "Help")
(setq resources-menu-sw-customize
      "Customise")
(setq resources-menu-sw-customize-smallworld
      "Customise Smallworld Emacs")
(setq resources-menu-sw-help-info
      "Help using Info")
(setq resources-menu-sw-help-htmlhelp
      "Help using HTMLHelp")
(setq resources-menu-sw-help-sw-menu
      "SW Menu Help")
(setq resources-menu-sw-help-keys
      "Smallworld Emacs Keys")
(setq resources-menu-sw-help-toggle
      "Prefer HTML Help")
(setq resources-menu-sw-help-view-file
      "View %s")
(setq resources-module-menu
      "Module")
(setq resources-module-menu-load-module
      "Load Module")
(setq resources-module-menu-reload-definition
      "Reload Module Definition")
(setq resources-module-menu-compile-messages
      "Compile Module Messages")
(setq resources-module-menu-remove-module
      "Remove Module")
(setq resources-module-menu-set-options
      "Set Options...")
(setq resources-module-menu-option-save-magikc-false
      "Set :save_magikc? to _false")
(setq resources-module-menu-option-save-magikc-true
      "Set :save_magikc? to _true")
(setq resources-module-menu-option-force-reload-false
      "Set :force_reload? to _false")
(setq resources-module-menu-option-force-reload-prerequisites
      "Set :force_reload? to :prerequisites")
(setq resources-module-menu-option-force-reload-true
      "Set :force_reload? to _true")
(setq resources-module-option-save-magikc-set
      "Set :save_magikc? to %s")
(setq resources-module-option-force-reload-set
      "Set :force_reload? option to %s")
(setq resources-mpde-menu
      "MPDE")
(setq resources-mpde-menu-set
      "Set Configuration")
(setq resources-mpde-menu-refresh
      "Refresh")
(setq resources-mpde-menu-upload-submissions
      "Upload Submissions")
(setq resources-mpde-menu-available
      "Available Configurations")
(setq resources-mpde-menu-display-configuration
      "Display Configuration")
(setq resources-mpde-menu-display-resource
      "Display Resource")
(setq resources-mpde-menu-make
      "Make Configuration")
(setq resources-mpde-menu-delete
      "Delete Configuration")
(setq resources-mpde-menu-install
      "Install MPDE in .emacs")
(setq resources-mpde-menu-dired-upload
      "SW Upload...")
(setq resources-mpde-menu-dired-upload-help
      "SW Upload current file or all marked files")
(setq resources-mpde-available-configurations
      "Magik Patch Development Environment Configurations")
(setq resources-mpde-loading-configuration
      "MPDE: Loading configuration settings for %s")
(setq resources-mpde-refreshing-configuration
      "Refreshing Configuration: %s...")
(setq resources-mpde-upload-submissions
      "Mark the files to upload then press the U key.")
(setq resources-mpde-refreshing-directories
      "directories")
(setq resources-mpde-refreshing-files
      "files")
(setq resources-mpde-refreshing-data
      "data")
(setq resources-mpde-refreshing-boot
      "boot")
(setq resources-mpde-refreshing-done
      "done")
(setq resources-mpde-refreshing-failed
      "done with error")
(setq resources-mpde-yn-refresh
      "Refresh %s configuration from master?")
(setq resources-mpde-yn-reload-elisp
      "Load the refreshed Smallworld Lisp code in this session?")
(setq resources-mpde-yn-overwrite-dotmpde
      "File exists ~/.mpde. Overwrite and loose all configuration definitions?")
(setq resources-mpde-yn-install-mpde
      "Your .emacs file already includes MPDE configuration data. Continue?")
(setq resources-mpde-yn-create-root-directory
      "The root directory %s does not exist, create it now?")
(setq resources-mpde-yn-delete-configuration
      "Deleting %s. Are you sure?")
(setq resources-mpde-source-dir
      "source     :")
(setq resources-mpde-destination-dir
      "destination:")
(setq resources-mpde-copied-file-to
      "Copied file %s to %s...")
(setq resources-mpde-copied-files
      "Copied files in directory %s to %s...")
(setq resources-mpde-copied-files-matching
      "Copied files in directory %s matching %s to %s...")
(setq resources-mpde-creating-file
      "Creating file %s")
(setq resources-mpde-storing-file
      "Storing file %s")
(setq resources-mpde-created-directory
      "Created directory %s")
(setq resources-mpde-copying-directory-to
      "Copied directory %s to %s...")
(setq resources-mpde-copying-files
      "Copying files in directory %s to %s...")
(setq resources-mpde-copying-files-matching
      "Copying files in directory %s matching %s to %s...")
(setq resources-mpde-configuration-prompt
      "Configuration name:")
(setq resources-mpde-configuration-default
      "local")
(setq resources-mpde-configuration-description-prompt
      "Description:")
(setq resources-mpde-configuration-description-default
      "My %s setup")
(setq resources-mpde-configuration-root-dir
      "%s root directory:")
(setq resources-mpde-resource-variables
      "Resource variables")
(setq resources-mpde-resource-variable
      "Resource variable")
(setq resources-mpde-resource-deprecated
      "(DEPRECATED)")
(setq resources-mpde-resource-documentation
      "Documentation:")
(setq resources-mpde-resource-action
      "ACTION:")
(setq resources-mpde-resource-current-value
      "Current value is =")
(setq resources-mpde-resource-no-docstring
      "No documentation available.")
(setq resources-mpde-action-void-variable
      "MPDE: Void variable %s, ignoring...")
(setq resources-mpde-action-no-action
      "MPDE: No action defined for variable %s")
(setq resources-mpde-debug
      "DEBUG:")
(setq resources-mpde-removed-entry
      "MPDE: removed %s entry from ~/.mpde")
(setq resources-mpde-refresh-master-error
      "Cannot refresh the master configuration!")
(setq resources-mpde-save-overwrite-error
      "Saving settings from 'emacs -q' would overwrite existing customizations")
(setq resources-mpde-copy-dir-error
      "Directory copy failed with exit status: %s")
(setq resources-mpde-delete-dir-error
      "Deletion of %s failed with exit status: %s")
(setq resources-mpde-configuration-unknown-error
      "Configuration %s is not defined")
(setq resources-mpde-no-master-error
      "Master configuration is not accessible")
(setq resources-mpde-no-master-template-error
      "Master template file %s does not exist")
(setq resources-mpde-no-boot-library-error
      "Master boot library %s does not exist")
(setq resources-mpde-already-installed-error
      "Aborted since your .emacs file includes MPDE configuration data")
(setq resources-mpde-not-master-error
      "Current configuration is not Master")
(setq resources-mpde-no-parent-directory-error
      "Current configuration is not Master")
(setq resources-mpde-delete-master-error
      "You cannot delete the master configuration!")
(setq resources-mpde-delete-current-error
      "You cannot delete the current configuration: %s!")
(setq resources-mpde-delete-abort-error
      "Deletion of %s aborted")
(setq resources-mpde-failed-validation-error
      "Cannot complete '%s' action. Offline configuration is %s")
(setq resources-mpde-loosing-changes-error
      "MPDE: Cannot save buffer without loosing changes at refresh")
(setq resources-mpde-file-exists-error
      "File exists %s")
(setq resources-msg-menu
      "Message")
(setq resources-msg-menu-transmit-buffer
      "Transmit Buffer")
(setq resources-msg-menu-compile-module
      "Compile Message File")
(setq resources-msg-menu-mark-message
      "Mark Message")
(setq resources-msg-menu-next
      "Next")
(setq resources-msg-menu-previous
      "Previous")
(setq resources-msg-loaded-in-buffer
      "%s loaded in buffer %s.")
(setq resources-msg-compile-module-in-buffer
      "Compiling all module messages in %s. ")
(setq resources-pragma-topic-select
      "Topic Select")
(setq resources-pragma-deprecated-template
      "Use toggle keys, \\\\ and /, on 'Action' line to choose action.")
(setq resources-pragma-yn-deprecated-modify-comments
      "Remove modifed deprecated comments?")
(setq resources-pragma-no-smallworld-gis-env
      "There is no value for $SMALLWORLD_GIS")
(setq resources-product-menu
      "Product")
(setq resources-product-menu-add-product
      "Add Product")
(setq resources-product-menu-reinitialise-product
      "Reinitialise Product")
(setq resources-load-resource-dir
      "Loading resources %s.")
(setq resources-entry-compile-dir
      "Resources dir:")
(setq resources-compile-resource-dir
      "Compiling Resource %s...")
(setq resources-compile-resource-done
      "Compiling Resources ... done")
(setq resources-compile-buffer-error
      "This is not an Emacs msg file.")
(setq resources-sw-find-tag-default-prompt
      "%s (default %s)")
(setq resources-sw-yn-buffer-modified
      "Buffer for filename %s modified - save changes?")
(setq resources-sw-yn-continue
      "Continue?")
(setq resources-sw-help-info
      "Emacs help displayed using Info")
(setq resources-sw-help-htmlhelp
      "Emacs help displayed using HTML Help")
(setq resources-sw-help-no-htmlhelp-file-error
      "Failed to locate HTML Help file '%s'")
(setq resources-sw-buffer-modified-error
      "Abandoned!")
(setq resources-sw-no-gis-process-error
      "There is no GIS process running in buffer '%s'")
(setq resources-sw-view-load-path-file-error
      "Impossible Error! Cannot find '%s' in 'load-path'")
(setq resources-sw-no-directory-error
      "Directory does not exist: %s")
(setq resources-sw-no-process-error
      "The '%s' buffer has no process running")
(setq resources-template-type-prompt
      "Magik %s type:")
(setq resources-template-copying-file
      "Copying template file %s to %s...")
(setq resources-template-copied-file
      "Copied template file %s to %s...done")
(setq resources-template-no-header-error
      "The template file, %s, doesn't seem to have column-1 hash, #, character")

(provide 'resources.msgc)
;;; resources.msgc.el ends here
