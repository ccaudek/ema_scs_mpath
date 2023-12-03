# %% Logging information --------------------------------------------

import datetime


onstart:
    current_time = datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
    shell(f"echo -e 'running\t{current_time}' > pipeline_status.txt")
    print(f"Workflow started at {current_time}.")


onsuccess:
    shell("echo -e 'success\t'$(date '+%Y-%m-%d %H:%M') > pipeline_status.txt")
    current_time = datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
    print(f"🎉 Workflow completed successfully at {current_time}.")


onerror:
    shell("echo -e 'error\t'$(date '+%Y-%m-%d %H:%M') > pipeline_status.txt")
    current_time = datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
    print(f"⛔️ An error occurred at {current_time}.")
    print("Please consult the log file at '.snakemake/log/' for more information.")
    print(
        "Consider rerunning with '--verbose' or '--debug' for detailed diagnostic information."
    )
