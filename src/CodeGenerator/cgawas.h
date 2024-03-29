void initialize_write_assembly (FILE *ass_file);
void write_assembly (VOID);
void w_as_abc_string_and_label_in_code_section (char *string,int length,char *label_name);
void w_as_abc_string_in_code_section (char *string,int length,int label_number);
void w_as_c_string_in_code_section (char *string,int length,int label_number);
void w_as_c_string_in_data_section (char *string,int length);
void w_as_descriptor_string_in_code_section (char *string,int length,int string_code_label_id,LABEL *string_label);
void w_as_word_in_data_section (int n);
void w_as_label_in_data_section (char *label_name);
#ifdef MACH_O64
void w_as_label_offset_in_data_section (char *label_name);
#endif
void w_as_descriptor_in_data_section (char *label_name);
void w_as_define_label (LABEL *label);
void w_as_internal_label_value (int label_id);
#ifdef MACH_O64
void w_as_internal_label_value_offset (int label_id);
#endif
void w_as_new_module (int flag);
void w_as_long_in_data_section (int n);
void w_as_to_data_section (void);
void w_as_descriptor_in_code_section (char *label_name);
void w_as_define_data_label (int label_number);
#ifdef MACH_O64
void w_as_align_and_define_data_label (int label_number);
#endif
void w_as_labeled_c_string_in_data_section (char *string,int length,int label_number);
void w_as_abc_string_in_data_section (char *string,int length);
void w_as_descriptor_string_in_data_section (char *string,int length,int string_label_id,LABEL *string_label);
void w_as_abc_string_and_label_in_data_section (char *string,int length,char *label_name);
void w_as_c_string_and_label_in_code_section (char *string,int length,char *label_name);
#ifdef _WINDOWS_
void w_as_new_data_module (void);
#endif
