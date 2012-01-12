
#include <stdlib.h>
#include <ctype.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "interfaces.h"
#include "scripts.h"
#include "app_script.h"

#if HW_HAS_BEEPER
VSS_HANDLER(app_beeper_init);
VSS_HANDLER(app_beeper_on);
VSS_HANDLER(app_beeper_off);

static struct vss_cmd_t beeper_cmd[] =
{
	VSS_CMD(	"init",
				"initialize beeper, format: beeper.init",
				app_beeper_init,
				NULL),
	VSS_CMD(	"on",
				"set beeper on, format: beeper on",
				app_beeper_on,
				NULL),
	VSS_CMD(	"off",
				"set beeper off, format: beeper.off",
				app_beeper_off,
				NULL),
};
#endif

#if HW_HAS_LEDARRAY
VSS_HANDLER(app_ledarr_init);
VSS_HANDLER(app_ledarr_set);

static struct vss_cmd_t ledarr_cmd[] =
{
	VSS_CMD(	"init",
				"initialize led array, format: ledarr.init",
				app_ledarr_init,
				NULL),
	VSS_CMD(	"set",
				"set led array value, format: ledarr.set VALUE",
				app_ledarr_set,
				NULL),
};
#endif

static struct vss_cmd_t app_cmd[] =
{
#if HW_HAS_BEEPER
	VSS_CMD(	"beeper",
				"beeper handler",
				app_beeper_init,
				beeper_cmd),
#endif
#if HW_HAS_LEDARRAY
	VSS_CMD(	"ledarr",
				"led array handler",
				app_ledarr_init,
				ledarr_cmd),
#endif
	VSS_CMD_END
};

struct vss_cmd_list_t app_cmd_list = VSS_CMD_LIST("app", app_cmd);

#if HW_HAS_BEEPER
VSS_HANDLER(app_beeper_init)
{
	VSS_CHECK_ARGC(1);
	BEEPER_INIT();
	return VSFERR_NONE;
}

VSS_HANDLER(app_beeper_on)
{
	VSS_CHECK_ARGC(1);
	BEEPER_ON();
	return VSFERR_NONE;
}

VSS_HANDLER(app_beeper_off)
{
	VSS_CHECK_ARGC(1);
	BEEPER_OFF();
	return VSFERR_NONE;
}
#endif

#if HW_HAS_LEDARRAY
VSS_HANDLER(app_ledarr_init)
{
	VSS_CHECK_ARGC(1);
	LED_ARRAY_INIT();
	return VSFERR_NONE;
}

VSS_HANDLER(app_ledarr_set)
{
	uint8_t value;
	
	VSS_CHECK_ARGC(2);
	value = (uint8_t)strtoul(argv[1], NULL, 0);
	LED_ARRAY_SET(value);
	return VSFERR_NONE;
}
#endif
