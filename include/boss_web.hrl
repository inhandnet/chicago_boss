
-record(boss_app_info, {
        application,
        base_url,
        init_data,
        router_sup_pid,
        router_pid,
		model_modules = [],
        controller_modules = []
    }).

