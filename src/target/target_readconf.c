/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"

#include <libxml/parser.h>

#include "vsprog.h"
#include "prog_interface.h"
#include "bufffunc.h"
#include "strparser.h"
#include "target.h"

#define TARGET_CONF_FILE_EXT			".xml"

static uint32_t target_xml_get_child_number(xmlNodePtr parentNode, 
												char * child_name)
{
	uint32_t result = 0;
	
	if ((parentNode != NULL) && (parentNode->children != NULL))
	{
		parentNode = parentNode->children->next;
		while (parentNode != NULL)
		{
			if (!xmlStrcmp(parentNode->name, BAD_CAST child_name))
			{
				result++;
			}
			parentNode = parentNode->next->next;
		}
	}
	
	return result;
}

RESULT target_build_chip_fl(const char *chip_series, 
				const char *chip_module, char *type, struct chip_fl_t *fl)
{
	xmlDocPtr doc = NULL;
	xmlNodePtr curNode = NULL;
	xmlNodePtr paramNode, settingNode;
	char *filename = NULL;
	uint32_t i, j, num_of_chips;
	RESULT ret = ERROR_OK;
	FILE *fp;
	char *format;
	char format_tmp[32];
	uint8_t size;
	
#if PARAM_CHECK
	if ((NULL == chip_series) || (NULL == chip_module) || (NULL == fl))
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	if (NULL == config_dir)
	{
		return ERROR_FAIL;
	}
	
	// release first if necessary
	target_release_chip_fl(fl);
	
	filename = (char *)malloc(strlen(config_dir)
					+ strlen(chip_series) + strlen(TARGET_CONF_FILE_EXT) + 1);
	if (NULL == filename)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return ERRCODE_NOT_ENOUGH_MEMORY;
	}
	strcpy(filename, config_dir);
	strcat(filename, chip_series);
	strcat(filename, TARGET_CONF_FILE_EXT);
	fp = fopen(filename, "r");
	if (NULL == fp)
	{
		// no error message, just return error
		ret = ERROR_FAIL;
		goto free_and_exit;
	}
	else
	{
		fclose(fp);
		fp = NULL;
	}
	
	doc = xmlReadFile(filename, "", XML_PARSE_RECOVER);
	if (NULL == doc)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPEN, filename);
		ret = ERRCODE_FAILURE_OPEN;
		goto free_and_exit;
	}
	curNode = xmlDocGetRootElement(doc);
	if (NULL == curNode)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
		ret = ERRCODE_FAILURE_OPERATION;
		goto free_and_exit;
	}
	
	// valid check
	if (xmlStrcmp(curNode->name, BAD_CAST "series") 
		|| !xmlHasProp(curNode, BAD_CAST "name") 
		|| xmlStrcmp(xmlGetProp(curNode, BAD_CAST "name"), 
					 (const xmlChar *)chip_series))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
		ret = ERRCODE_FAILURE_OPERATION;
		goto free_and_exit;
	}
	
	num_of_chips = target_xml_get_child_number(curNode, "chip");
	if (0 == num_of_chips)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, chip_series);
		ret = ERRCODE_NOT_SUPPORT;
		goto free_and_exit;
	}
	
	// read data
	curNode = curNode->children->next;
	for (i = 0; i < num_of_chips; i++)
	{
		// check
		if ((NULL == curNode) 
			|| xmlStrcmp(curNode->name, BAD_CAST "chip")
			|| !xmlHasProp(curNode, BAD_CAST "name"))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
			ret = ERRCODE_FAILURE_OPERATION;
			goto free_and_exit;
		}
		
		if (strcmp((const char *)chip_module, 
				   (const char *)xmlGetProp(curNode, BAD_CAST "name")))
		{
			// not the chip I want
			curNode = curNode->next->next;
			continue;
		}
		else
		{
			break;
		}
	}
	if (i >= num_of_chips)
	{
		// not found
		goto free_and_exit;
	}
	
	paramNode = curNode->children->next;
	// read parameters
	while((paramNode != NULL) && xmlStrcmp(paramNode->name, BAD_CAST type))
	{
		paramNode = paramNode->next->next;
	}
	if (NULL == paramNode)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, type, chip_module);
		ret = ERRCODE_NOT_SUPPORT;
		goto free_and_exit;
	}
	
	// we found the parameter
	// valid check
	if (!xmlHasProp(paramNode, BAD_CAST "init") 
		|| !xmlHasProp(paramNode, BAD_CAST "bytesize"))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
		ret = ERRCODE_FAILURE_OPERATION;
		goto free_and_exit;
	}
	size = (uint8_t)strtoul((char*)xmlGetProp(paramNode, BAD_CAST "bytesize"), NULL, 0);
	format = (char *)xmlGetProp(paramNode, BAD_CAST "format");
	if (NULL == format)
	{
		if (size > 8)
		{
			LOG_ERROR(ERRMSG_NOT_DEFINED, "format node");
			ret = ERROR_FAIL;
			goto free_and_exit;
		}
		snprintf(format_tmp, sizeof(format_tmp), "%%%dx", size);
		format = format_tmp;
	}
	
	// read fl number
	fl->num_of_fl_settings = 
		(uint16_t)target_xml_get_child_number(paramNode, "setting");
	if (0 == fl->num_of_fl_settings)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, type, chip_module);
		ret = ERRCODE_NOT_SUPPORT;
		goto free_and_exit;
	}
	
	// read fl init value
	if (NULL == bufffunc_malloc_and_copy_str(&fl->init_value, 
							(char *)xmlGetProp(paramNode, BAD_CAST "init")))
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		ret = ERRCODE_NOT_ENOUGH_MEMORY;
		goto free_and_exit;
	}
	if (ERROR_OK != strparser_check(fl->init_value, format))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse init node");
		ret = ERRCODE_FAILURE_OPERATION;
		goto free_and_exit;
	}
	
	// alloc memory for settings
	fl->settings = (struct chip_fl_setting_t*)malloc(
		fl->num_of_fl_settings * sizeof(struct chip_fl_setting_t));
	if (NULL == fl->settings)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		ret = ERRCODE_NOT_ENOUGH_MEMORY;
		goto free_and_exit;
	}
	memset(fl->settings, 0, 
		   fl->num_of_fl_settings * sizeof(struct chip_fl_setting_t));
	
	settingNode = paramNode->children->next;
	// has warning?
	if ((settingNode != NULL) && 
		!strcmp((const char *)settingNode->name, "warning"))
	{
		xmlNodePtr warningNode = settingNode;
		xmlNodePtr wNode;
		
		settingNode = settingNode->next->next;
		// parse warning
		fl->num_of_fl_warnings = 
			(uint16_t)target_xml_get_child_number(warningNode, "w");
		if (fl->num_of_fl_warnings != 0)
		{
			fl->warnings = (struct chip_fl_warning_t*)malloc(
				fl->num_of_fl_warnings * sizeof(struct chip_fl_warning_t));
			if (NULL == fl->warnings)
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				ret = ERRCODE_NOT_ENOUGH_MEMORY;
				goto free_and_exit;
			}
			memset(fl->warnings, 0, 
				   fl->num_of_fl_warnings * sizeof(struct chip_fl_warning_t));
			
			wNode = warningNode->children->next;
			for (i = 0; i < fl->num_of_fl_warnings; i++)
			{
				// check
				if (strcmp((const char *)wNode->name, "w"))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
					ret = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
				
				if (NULL == bufffunc_malloc_and_copy_str(
						&fl->warnings[i].mask, 
						(char *)xmlGetProp(wNode, BAD_CAST "mask")))
				{
					LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
					ret = ERRCODE_NOT_ENOUGH_MEMORY;
					goto free_and_exit;
				}
				if (ERROR_OK != strparser_check(fl->warnings[i].mask, format))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse mask node");
					ret = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
				
				if (NULL == bufffunc_malloc_and_copy_str(
						&fl->warnings[i].value, 
						(char *)xmlGetProp(wNode, BAD_CAST "value")))
				{
					LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
					ret = ERRCODE_NOT_ENOUGH_MEMORY;
					goto free_and_exit;
				}
				if (ERROR_OK != strparser_check(fl->warnings[i].value, format))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse value node");
					ret = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
				
				if (NULL == bufffunc_malloc_and_copy_str(
						&fl->warnings[i].msg, 
						(char *)xmlGetProp(wNode, BAD_CAST "msg")))
				{
					LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
					ret = ERRCODE_NOT_ENOUGH_MEMORY;
					goto free_and_exit;
				}
				
				fl->warnings[i].ban = 0;
				if (xmlHasProp(wNode, BAD_CAST "ban"))
				{
					fl->warnings[i].ban = (uint8_t)strtoul(
						(const char *)xmlGetProp(wNode, BAD_CAST "ban"), 
						NULL, 0);
				}
				
				wNode = wNode->next->next;
			}
		}
	}
	
	// parse settings
	for (i = 0; i < fl->num_of_fl_settings; i++)
	{
		xmlNodePtr choiceNode;
		
		fl->settings[i].num_of_choices = 
			(uint16_t)target_xml_get_child_number(settingNode, "choice");
		// check
		if (strcmp((const char *)settingNode->name, "setting") 
			|| (!xmlHasProp(settingNode, BAD_CAST "name"))
			|| (!xmlHasProp(settingNode, BAD_CAST "mask")))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
			ret = ERRCODE_FAILURE_OPERATION;
			goto free_and_exit;
		}
		
		if (NULL == bufffunc_malloc_and_copy_str(
						&fl->settings[i].name, 
						(char *)xmlGetProp(settingNode, BAD_CAST "name")))
		{
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
			ret = ERRCODE_NOT_ENOUGH_MEMORY;
			goto free_and_exit;
		}
		
		if (NULL == bufffunc_malloc_and_copy_str(
						&fl->settings[i].mask, 
						(char *)xmlGetProp(settingNode, BAD_CAST "mask")))
		{
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
			ret = ERRCODE_NOT_ENOUGH_MEMORY;
			goto free_and_exit;
		}
		if (ERROR_OK != strparser_check(fl->settings[i].mask, format))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse mask node");
			ret = ERRCODE_FAILURE_OPERATION;
			goto free_and_exit;
		}
		
		if (xmlHasProp(settingNode, BAD_CAST "ban"))
		{
			if (NULL == bufffunc_malloc_and_copy_str(
						&fl->settings[i].ban, 
						(char *)xmlGetProp(settingNode, BAD_CAST "ban")))
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				ret = ERRCODE_NOT_ENOUGH_MEMORY;
				goto free_and_exit;
			}
		}
		
		// parse info if exists
		if (xmlHasProp(settingNode, BAD_CAST "info"))
		{
			if (NULL == bufffunc_malloc_and_copy_str(
						&fl->settings[i].info, 
						(char *)xmlGetProp(settingNode, BAD_CAST "info")))
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				ret = ERRCODE_NOT_ENOUGH_MEMORY;
				goto free_and_exit;
			}
		}
		
		// parse format if exists
		if (xmlHasProp(settingNode, BAD_CAST "format"))
		{
			if (NULL == bufffunc_malloc_and_copy_str(
						&fl->settings[i].format, 
						(char *)xmlGetProp(settingNode, BAD_CAST "format")))
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				ret = ERRCODE_NOT_ENOUGH_MEMORY;
				goto free_and_exit;
			}
		}
		
		// parse bytelen if exists
		if (xmlHasProp(settingNode, BAD_CAST "bytelen"))
		{
			fl->settings[i].bytelen = (uint8_t)strtoul(
				(const char *)xmlGetProp(settingNode, BAD_CAST "bytelen"), 
				NULL, 0);
		}
		
		// parse radix if exists
		if (xmlHasProp(settingNode, BAD_CAST "radix"))
		{
			fl->settings[i].radix = (uint8_t)strtoul(
				(const char *)xmlGetProp(settingNode, BAD_CAST "radix"), 
				NULL, 0);
		}
		
		// parse shift if exists
		if (xmlHasProp(settingNode, BAD_CAST "shift"))
		{
			fl->settings[i].shift = (uint8_t)strtoul(
				(const char *)xmlGetProp(settingNode, BAD_CAST "shift"), 
				NULL, 0);
		}
		
		// parse checked or unchecked
		if (xmlHasProp(settingNode, BAD_CAST "checked"))
		{
			if (NULL == bufffunc_malloc_and_copy_str(
						&fl->settings[i].checked, 
						(char *)xmlGetProp(settingNode, BAD_CAST "checked")))
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				ret = ERRCODE_NOT_ENOUGH_MEMORY;
				goto free_and_exit;
			}
			if (ERROR_OK != strparser_check(fl->settings[i].checked, format))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse checked node");
				ret = ERRCODE_FAILURE_OPERATION;
				goto free_and_exit;
			}
			if (!xmlHasProp(settingNode, BAD_CAST "unchecked"))
			{
				uint64_t val_tmp, mask_tmp;
				
				if (size > 8)
				{
					LOG_ERROR(ERRMSG_NOT_DEFINED, "unchecked node");
					ret = ERROR_FAIL;
					goto free_and_exit;
				}
				val_tmp = 0;
				if (ERROR_OK != strparser_parse(fl->settings[i].checked, 
							format, (uint8_t*)&val_tmp, sizeof(val_tmp)))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse checked node");
					ret = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
				mask_tmp = 0;
				if (ERROR_OK != strparser_parse(fl->settings[i].mask, 
							format, (uint8_t*)&mask_tmp, sizeof(mask_tmp)))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse mask node");
					ret = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
				val_tmp ^= mask_tmp;
				fl->settings[i].unchecked = 
					strparser_solve(format, (uint8_t*)&val_tmp, 
									sizeof(val_tmp));
				if (NULL == fl->settings[i].unchecked)
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, 
								"solve unchecked value");
					ret = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
			}
			fl->settings[i].use_checkbox = 1;
		}
		if (xmlHasProp(settingNode, BAD_CAST "unchecked"))
		{
			if (NULL == bufffunc_malloc_and_copy_str(
						&fl->settings[i].unchecked, 
						(char *)xmlGetProp(settingNode, BAD_CAST "unchecked")))
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				ret = ERRCODE_NOT_ENOUGH_MEMORY;
				goto free_and_exit;
			}
			if (ERROR_OK != strparser_check(fl->settings[i].unchecked, format))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse unchecked node");
				ret = ERRCODE_FAILURE_OPERATION;
				goto free_and_exit;
			}
			if (!xmlHasProp(settingNode, BAD_CAST "checked"))
			{
				uint64_t val_tmp, mask_tmp;
				
				if (size > 8)
				{
					LOG_ERROR(ERRMSG_NOT_DEFINED, "checked node");
					ret = ERROR_FAIL;
					goto free_and_exit;
				}
				val_tmp = 0;
				if (ERROR_OK != strparser_parse(fl->settings[i].unchecked, 
							format, (uint8_t*)&val_tmp, sizeof(val_tmp)))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse unchecked node");
					ret = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
				mask_tmp = 0;
				if (ERROR_OK != strparser_parse(fl->settings[i].mask, 
							format, (uint8_t*)&mask_tmp, sizeof(mask_tmp)))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse mask node");
					ret = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
				val_tmp ^= mask_tmp;
				fl->settings[i].checked = 
					strparser_solve(format, (uint8_t*)&val_tmp, 
									sizeof(val_tmp));
				if (NULL == fl->settings[i].checked)
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "solve checked value");
					ret = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
			}
			fl->settings[i].use_checkbox = 1;
		}
		
		if (!fl->settings[i].use_checkbox 
			&& (0 == fl->settings[i].num_of_choices))
		{
			fl->settings[i].use_edit = 1;
		}
		else
		{
			fl->settings[i].use_edit = 0;
		}
		
		// parse choices
		if (0 == fl->settings[i].num_of_choices)
		{
			// no choice
			settingNode = settingNode->next->next;
			continue;
		}
		
		// malloc memory for choices
		fl->settings[i].choices = (struct chip_fl_choice_t*)malloc(
			fl->settings[i].num_of_choices * sizeof(struct chip_fl_choice_t));
		if (NULL == fl->settings[i].choices)
		{
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
			ret = ERRCODE_NOT_ENOUGH_MEMORY;
			goto free_and_exit;
		}
		memset(fl->settings[i].choices, 0, 
			fl->settings[i].num_of_choices * sizeof(struct chip_fl_choice_t));
		
		choiceNode = settingNode->children->next;
		// parse choices
		for (j = 0; j < fl->settings[i].num_of_choices; j++)
		{
			// check
			if (strcmp((const char *)choiceNode->name, "choice") 
				|| !xmlHasProp(choiceNode, BAD_CAST "value") 
				|| !xmlHasProp(choiceNode, BAD_CAST "text"))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
				ret = ERRCODE_FAILURE_OPERATION;
				goto free_and_exit;
			}
			
			// parse
			if (NULL == bufffunc_malloc_and_copy_str(
						&fl->settings[i].choices[j].value, 
						(char *)xmlGetProp(choiceNode, BAD_CAST "value")))
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				ret = ERRCODE_NOT_ENOUGH_MEMORY;
				goto free_and_exit;
			}
			if (ERROR_OK != strparser_check(fl->settings[i].choices[j].value, 
												format))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse value node");
				ret = ERRCODE_FAILURE_OPERATION;
				goto free_and_exit;
			}
			
			if (NULL == bufffunc_malloc_and_copy_str(
						&fl->settings[i].choices[j].text, 
						(char *)xmlGetProp(choiceNode, BAD_CAST "text")))
			{
				LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
				ret = ERRCODE_NOT_ENOUGH_MEMORY;
				goto free_and_exit;
			}
			
			choiceNode = choiceNode->next->next;
		}
		settingNode = settingNode->next->next;
	}
	
free_and_exit:
	if (filename != NULL)
	{
		free(filename);
		filename = NULL;
	}
	if (doc != NULL)
	{
		xmlFreeDoc(doc);
		doc = NULL;
	}
	
	return ret;
}

RESULT target_build_chip_series(const char *chip_series, 
		const struct program_mode_t *program_mode, struct chip_series_t *s)
{
	xmlDocPtr doc = NULL;
	xmlNodePtr curNode = NULL;
	char *filename = NULL;
	struct chip_param_t *p_param;
	uint32_t i, j, k, target_para_size_defined;
	RESULT ret = ERROR_OK;
	FILE *fp;
	
#if PARAM_CHECK
	if ((NULL == chip_series) || (NULL == s))
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	if (NULL == config_dir)
	{
		return ERROR_FAIL;
	}
	
	// release first if necessary
	target_release_chip_series(s);
	
	filename = (char *)malloc(strlen(config_dir)+ strlen(chip_series) 
								+ strlen(TARGET_CONF_FILE_EXT) + 1);
	if (NULL == filename)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return ERRCODE_NOT_ENOUGH_MEMORY;
	}
	strcpy(filename, config_dir);
	strcat(filename, chip_series);
	strcat(filename, TARGET_CONF_FILE_EXT);
	fp = fopen(filename, "r");
	if (NULL == fp)
	{
		// no error message, just return error
		ret = ERROR_FAIL;
		goto free_and_exit;
	}
	else
	{
		fclose(fp);
		fp = NULL;
	}
	
	doc = xmlReadFile(filename, "", XML_PARSE_RECOVER);
	if (NULL == doc)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPEN, filename);
		ret = ERRCODE_FAILURE_OPEN;
		goto free_and_exit;
	}
	curNode = xmlDocGetRootElement(doc);
	if (NULL == curNode)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
		ret = ERRCODE_FAILURE_OPERATION;
		goto free_and_exit;
	}
	
	// valid check
	if (xmlStrcmp(curNode->name, BAD_CAST "series") 
		|| !xmlHasProp(curNode, BAD_CAST "name") 
		|| xmlStrcmp(xmlGetProp(curNode, BAD_CAST "name"), 
					 (const xmlChar *)chip_series))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
		ret = ERRCODE_FAILURE_OPERATION;
		goto free_and_exit;
	}
	
	s->num_of_chips = target_xml_get_child_number(curNode, "chip");
	if (0 == s->num_of_chips)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, chip_series);
		ret = ERRCODE_NOT_SUPPORT;
		goto free_and_exit;
	}
	s->chips_param = (struct chip_param_t *)malloc(sizeof(struct chip_param_t) 
											* s->num_of_chips);
	if (NULL == s->chips_param)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		ret = ERRCODE_NOT_ENOUGH_MEMORY;
		goto free_and_exit;
	}
	memset(s->chips_param, 0, sizeof(struct chip_param_t) * s->num_of_chips);
	
	// read data
	curNode = curNode->children->next;
	for (i = 0; i < s->num_of_chips; i++)
	{
		xmlNodePtr paramNode;
		uint32_t size;
		char *format, *str;
		char format_tmp[32];
		uint8_t *buff;
		
		p_param = &(s->chips_param[i]);
		
		// check
		if ((NULL == curNode) 
			|| xmlStrcmp(curNode->name, BAD_CAST "chip")
			|| !xmlHasProp(curNode, BAD_CAST "name"))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read config file");
			ret = ERRCODE_FAILURE_OPERATION;
			goto free_and_exit;
		}
		
		// read name
		strncpy(p_param->chip_name, 
				(const char *)xmlGetProp(curNode, BAD_CAST "name"), 
				sizeof(p_param->chip_name));
		
		// read parameters
		target_para_size_defined = 0;
		paramNode = curNode->children->next;
		while(paramNode != NULL)
		{
			if (!xmlStrcmp(paramNode->name, BAD_CAST "chip_id"))
			{
				p_param->chip_id = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "chip_erase"))
			{
				p_param->chip_erase = (uint8_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "program_mode"))
			{
				char *mode_tmp = (char *)xmlNodeGetContent(paramNode);
				int8_t mode_idx;
				
				if (NULL == bufffunc_malloc_and_copy_str(
									&p_param->program_mode_str, mode_tmp))
				{
					LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
					ret = ERRCODE_NOT_ENOUGH_MEMORY;
					goto free_and_exit;
				}
				p_param->program_mode = 0;
				if ((0 != i) 
					|| (strcmp(chip_series, p_param->chip_name)))
				{
					for (j = 0; j < strlen(mode_tmp); j++)
					{
						mode_idx = 
								target_mode_get_idx(program_mode, mode_tmp[j]);
						if (mode_idx >= 0)
						{
							p_param->program_mode |= 1 << mode_idx;
						}
						else
						{
							LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, mode_tmp, 
										"current target");
							ret = ERRCODE_NOT_SUPPORT;
							goto free_and_exit;
						}
					}
				}
				else
				{
					j = 0;
					while (program_mode[j].name != 0)
					{
						p_param->program_mode |= 1 << j;
						j++;
					}
				}
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_addr"))
			{
				p_param->chip_areas[BOOTLOADER_IDX].addr = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_seg"))
			{
				p_param->chip_areas[BOOTLOADER_IDX].seg = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_page_size"))
			{
				p_param->chip_areas[BOOTLOADER_IDX].page_size = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_param->chip_areas[BOOTLOADER_IDX].size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_page_num"))
			{
				p_param->chip_areas[BOOTLOADER_IDX].page_num = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_param->chip_areas[BOOTLOADER_IDX].size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_default"))
			{
				p_param->chip_areas[BOOTLOADER_IDX].default_value = 
					(uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "sram_addr"))
			{
				p_param->chip_areas[SRAM_IDX].addr = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "sram_page_size"))
			{
				p_param->chip_areas[SRAM_IDX].page_size = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_param->chip_areas[SRAM_IDX].size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "sram_page_num"))
			{
				p_param->chip_areas[SRAM_IDX].page_num = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_param->chip_areas[SRAM_IDX].size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_addr"))
			{
				p_param->chip_areas[APPLICATION_IDX].addr = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_seg"))
			{
				p_param->chip_areas[APPLICATION_IDX].seg = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_page_size"))
			{
				p_param->chip_areas[APPLICATION_IDX].page_size = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_param->chip_areas[APPLICATION_IDX].size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_page_num"))
			{
				p_param->chip_areas[APPLICATION_IDX].page_num = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_param->chip_areas[APPLICATION_IDX].size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_default"))
			{
				p_param->chip_areas[APPLICATION_IDX].default_value = 
					(uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_addr"))
			{
				p_param->chip_areas[EEPROM_IDX].addr = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_seg"))
			{
				p_param->chip_areas[EEPROM_IDX].seg = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_page_size"))
			{
				p_param->chip_areas[EEPROM_IDX].page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_param->chip_areas[EEPROM_IDX].size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_page_num"))
			{
				p_param->chip_areas[EEPROM_IDX].page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_param->chip_areas[EEPROM_IDX].size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_default"))
			{
				p_param->chip_areas[EEPROM_IDX].default_value = 
					(uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_addr"))
			{
				p_param->chip_areas[OTPROM_IDX].addr = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_seg"))
			{
				p_param->chip_areas[OTPROM_IDX].seg = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_page_size"))
			{
				p_param->chip_areas[OTPROM_IDX].page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_param->chip_areas[OTPROM_IDX].size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_page_num"))
			{
				p_param->chip_areas[OTPROM_IDX].page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_param->chip_areas[OTPROM_IDX].size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_default"))
			{
				p_param->chip_areas[OTPROM_IDX].default_value = 
					(uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_addr"))
			{
				p_param->chip_areas[USRSIG_IDX].addr = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_seg"))
			{
				p_param->chip_areas[USRSIG_IDX].seg = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_page_size"))
			{
				p_param->chip_areas[USRSIG_IDX].page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_param->chip_areas[USRSIG_IDX].size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_page_num"))
			{
				p_param->chip_areas[USRSIG_IDX].page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_param->chip_areas[USRSIG_IDX].size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_default"))
			{
				p_param->chip_areas[USRSIG_IDX].default_value = 
					(uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_addr"))
			{
				p_param->chip_areas[FUSE_IDX].addr = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_seg"))
			{
				p_param->chip_areas[FUSE_IDX].seg = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_page_num"))
			{
				p_param->chip_areas[FUSE_IDX].page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_param->chip_areas[FUSE_IDX].size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_page_size"))
			{
				p_param->chip_areas[FUSE_IDX].page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_param->chip_areas[FUSE_IDX].size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_default"))
			{
				p_param->chip_areas[FUSE_IDX].default_value = 
					(uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_addr"))
			{
				p_param->chip_areas[LOCK_IDX].addr = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_seg"))
			{
				p_param->chip_areas[LOCK_IDX].seg = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_page_num"))
			{
				p_param->chip_areas[LOCK_IDX].page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_param->chip_areas[LOCK_IDX].size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_page_size"))
			{
				p_param->chip_areas[LOCK_IDX].page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				p_param->chip_areas[LOCK_IDX].size = 0;
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_default"))
			{
				p_param->chip_areas[LOCK_IDX].default_value = 
					(uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_size"))
			{
				target_para_size_defined |= FUSE;
				p_param->chip_areas[FUSE_IDX].size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_size"))
			{
				target_para_size_defined |= LOCK;
				p_param->chip_areas[LOCK_IDX].size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_size"))
			{
				target_para_size_defined |= BOOTLOADER;
				p_param->chip_areas[BOOTLOADER_IDX].size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "sram_size"))
			{
				target_para_size_defined |= SRAM;
				p_param->chip_areas[SRAM_IDX].size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_size"))
			{
				target_para_size_defined |= APPLICATION;
				p_param->chip_areas[APPLICATION_IDX].size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_size"))
			{
				target_para_size_defined |= EEPROM;
				p_param->chip_areas[EEPROM_IDX].size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_size"))
			{
				target_para_size_defined |= OTPROM;
				p_param->chip_areas[OTPROM_IDX].size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_size"))
			{
				target_para_size_defined |= USRSIG;
				p_param->chip_areas[USRSIG_IDX].size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse"))
			{
				target_para_size_defined |= FUSE;
				p_param->chip_areas[FUSE_IDX].size = (uint32_t)strtoul(
					(const char *)xmlGetProp(paramNode, BAD_CAST "bytesize"), 
					NULL, 0);
				size = p_param->chip_areas[FUSE_IDX].size;
				p_param->chip_areas[FUSE_IDX].default_value = strtoull(
					(const char *)xmlGetProp(paramNode, BAD_CAST "init"), 
					NULL, 0);
				p_param->chip_areas[FUSE_IDX].cli_format = NULL;
				p_param->chip_areas[FUSE_IDX].mask = NULL;
				str = (char *)xmlGetProp(paramNode, BAD_CAST "format");
				if (str != NULL)
				{
					if (NULL == bufffunc_malloc_and_copy_str(
						&p_param->chip_areas[FUSE_IDX].cli_format, str))
					{
						LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
						ret = ERRCODE_NOT_ENOUGH_MEMORY;
						goto free_and_exit;
					}
					format = p_param->chip_areas[FUSE_IDX].cli_format;
				}
				else
				{
					if (size > 8)
					{
						LOG_ERROR(ERRMSG_NOT_DEFINED, "format node");
						ret = ERROR_FAIL;
						goto free_and_exit;
					}
					snprintf(format_tmp, sizeof(format_tmp), "%%%dx", size);
					format = format_tmp;
				}
				str = (char *)xmlGetProp(paramNode, BAD_CAST "mask");
				if (str != NULL)
				{
					p_param->chip_areas[FUSE_IDX].mask = 
						(uint8_t *)malloc(size);
					if (NULL == p_param->chip_areas[FUSE_IDX].mask)
					{
						LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
						ret = ERRCODE_NOT_ENOUGH_MEMORY;
						goto free_and_exit;
					}
					buff = p_param->chip_areas[FUSE_IDX].mask;
					if (ERROR_OK != 
							strparser_parse(str, format, buff, size))
					{
						LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse mask node");
						ret = ERRCODE_FAILURE_OPERATION;
						goto free_and_exit;
					}
				}
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock"))
			{
				target_para_size_defined |= LOCK;
				p_param->chip_areas[LOCK_IDX].size = (uint32_t)strtoul(
					(const char *)xmlGetProp(paramNode, BAD_CAST "bytesize"), 
					NULL, 0);
				size = p_param->chip_areas[LOCK_IDX].size;
				p_param->chip_areas[LOCK_IDX].default_value = strtoull(
					(const char *)xmlGetProp(paramNode, BAD_CAST "init"), 
					NULL, 0);
				p_param->chip_areas[LOCK_IDX].cli_format = NULL;
				p_param->chip_areas[LOCK_IDX].mask = NULL;
				str = (char *)xmlGetProp(paramNode, BAD_CAST "format");
				if (str != NULL)
				{
					if (NULL == bufffunc_malloc_and_copy_str(
						&p_param->chip_areas[LOCK_IDX].cli_format, str))
					{
						LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
						ret = ERRCODE_NOT_ENOUGH_MEMORY;
						goto free_and_exit;
					}
					format = p_param->chip_areas[LOCK_IDX].cli_format;
				}
				else
				{
					if (size > 8)
					{
						LOG_ERROR(ERRMSG_NOT_DEFINED, "format node");
						ret = ERROR_FAIL;
						goto free_and_exit;
					}
					snprintf(format_tmp, sizeof(format_tmp), "%%%dx", size);
					format = format_tmp;
				}
				str = (char *)xmlGetProp(paramNode, BAD_CAST "mask");
				if (str != NULL)
				{
					p_param->chip_areas[LOCK_IDX].mask = 
						(uint8_t *)malloc(size);
					if (NULL == p_param->chip_areas[LOCK_IDX].mask)
					{
						LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
						ret = ERRCODE_NOT_ENOUGH_MEMORY;
						goto free_and_exit;
					}
					buff = p_param->chip_areas[LOCK_IDX].mask;
					if (ERROR_OK != 
							strparser_parse(str, format, buff, size))
					{
						LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse mask node");
						ret = ERRCODE_FAILURE_OPERATION;
						goto free_and_exit;
					}
				}
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "calibration"))
			{
				target_para_size_defined |= CALIBRATION;
				p_param->chip_areas[CALIBRATION_IDX].size = (uint32_t)strtoul(
					(const char *)xmlGetProp(paramNode,BAD_CAST "bytesize"),
					NULL, 0);
				size = p_param->chip_areas[CALIBRATION_IDX].size;
				p_param->chip_areas[CALIBRATION_IDX].default_value = strtoull(
						(const char *)xmlGetProp(paramNode, BAD_CAST "init"), 
						NULL, 0);
				p_param->chip_areas[CALIBRATION_IDX].cli_format = NULL;
				p_param->chip_areas[CALIBRATION_IDX].mask = NULL;
				str = (char *)xmlGetProp(paramNode, BAD_CAST "format");
				if (str != NULL)
				{
					if (NULL == bufffunc_malloc_and_copy_str(
						&p_param->chip_areas[CALIBRATION_IDX].cli_format, str))
					{
						LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
						ret = ERRCODE_NOT_ENOUGH_MEMORY;
						goto free_and_exit;
					}
					format = p_param->chip_areas[CALIBRATION_IDX].cli_format;
				}
				else
				{
					if (size > 8)
					{
						LOG_ERROR(ERRMSG_NOT_DEFINED, "format node");
						ret = ERROR_FAIL;
						goto free_and_exit;
					}
					snprintf(format_tmp, sizeof(format_tmp), "%%%dx", size);
					format = format_tmp;
				}
				str = (char *)xmlGetProp(paramNode, BAD_CAST "mask");
				if (str != NULL)
				{
					p_param->chip_areas[CALIBRATION_IDX].mask = 
						(uint8_t *)malloc(size);
					if (NULL == p_param->chip_areas[CALIBRATION_IDX].mask)
					{
						LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
						ret = ERRCODE_NOT_ENOUGH_MEMORY;
						goto free_and_exit;
					}
					buff = p_param->chip_areas[CALIBRATION_IDX].mask;
					if (ERROR_OK != 
							strparser_parse(str, format, buff, size))
					{
						LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse mask node");
						ret = ERRCODE_FAILURE_OPERATION;
						goto free_and_exit;
					}
				}
			}
			else
			{
				char *str_tmp = (char *)paramNode->name;
				
				if ((strlen(str_tmp) >= 6) 
					&& ('p' == str_tmp[0]) 
					&& ('a' == str_tmp[1])
					&& ('r' == str_tmp[2])
					&& ('a' == str_tmp[3])
					&& ('m' == str_tmp[4]))
				{
					// parameters
					j = strtoul(&str_tmp[5], NULL, 0);
					p_param->param[j] = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				}
				else
				{
					// wrong parameter
					LOG_ERROR(ERRMSG_INVALID, 
						(const char *)xmlNodeGetContent(paramNode), 
						chip_series);
					ret = ERRCODE_INVALID;
					goto free_and_exit;
				}
			}
			
			paramNode = paramNode->next->next;
		}
		
		if (!(target_para_size_defined & SRAM) 
			&& !p_param->chip_areas[SRAM_IDX].size)
		{
			p_param->chip_areas[SRAM_IDX].size = 
				p_param->chip_areas[SRAM_IDX].page_size 
				* p_param->chip_areas[SRAM_IDX].page_num;
		}
		if (!(target_para_size_defined & APPLICATION) 
			&& !p_param->chip_areas[APPLICATION_IDX].size)
		{
			p_param->chip_areas[APPLICATION_IDX].size = 
				p_param->chip_areas[APPLICATION_IDX].page_size 
				* p_param->chip_areas[APPLICATION_IDX].page_num;
		}
		if (!(target_para_size_defined & EEPROM) 
			&& !p_param->chip_areas[EEPROM_IDX].size)
		{
			p_param->chip_areas[EEPROM_IDX].size = 
				p_param->chip_areas[EEPROM_IDX].page_size 
				* p_param->chip_areas[EEPROM_IDX].page_num;
		}
		if (!(target_para_size_defined & BOOTLOADER) 
			&& !p_param->chip_areas[BOOTLOADER_IDX].size)
		{
			p_param->chip_areas[BOOTLOADER_IDX].size = 
				p_param->chip_areas[BOOTLOADER_IDX].page_size 
				* p_param->chip_areas[BOOTLOADER_IDX].page_num;
		}
		if (!(target_para_size_defined & FUSE) 
			&& !p_param->chip_areas[FUSE_IDX].size)
		{
			p_param->chip_areas[FUSE_IDX].size = 
				p_param->chip_areas[FUSE_IDX].page_size 
				* p_param->chip_areas[FUSE_IDX].page_num;
		}
		if (!(target_para_size_defined & LOCK) 
			&& !p_param->chip_areas[LOCK_IDX].size)
		{
			p_param->chip_areas[LOCK_IDX].size = 
				p_param->chip_areas[LOCK_IDX].page_size 
				* p_param->chip_areas[LOCK_IDX].page_num;
		}
		if (!(target_para_size_defined & OTPROM) 
			&& !p_param->chip_areas[OTPROM_IDX].size)
		{
			p_param->chip_areas[OTPROM_IDX].size = 
				p_param->chip_areas[OTPROM_IDX].page_size 
				* p_param->chip_areas[OTPROM_IDX].page_num;
		}
		if (!(target_para_size_defined & USRSIG) 
			&& !p_param->chip_areas[USRSIG_IDX].size)
		{
			p_param->chip_areas[USRSIG_IDX].size = 
				p_param->chip_areas[USRSIG_IDX].page_size 
				* p_param->chip_areas[USRSIG_IDX].page_num;
		}
		
		if (0 == i)
		{
			// first chip is used to setting every chip
			for (j = 1; j < s->num_of_chips; j++)
			{
				memcpy(&s->chips_param[j], &s->chips_param[0], 
							sizeof(struct chip_param_t));
				s->chips_param[j].program_mode_str = NULL;
				for (k = 0; k < dimof(target_area_name); k++)
				{
					s->chips_param[j].chip_areas[k].cli_format = NULL;
				}
			}
		}
		
		curNode = curNode->next->next;
	}
	
free_and_exit:
	if (filename != NULL)
	{
		free(filename);
		filename = NULL;
	}
	if (doc != NULL)
	{
		xmlFreeDoc(doc);
		doc = NULL;
	}
	
	return ret;
}

