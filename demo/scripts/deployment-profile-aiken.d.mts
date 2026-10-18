/** Profile field name to the Aiken env constant it renders as. */
type ConstantTable = Readonly<Record<string, string>>;

export declare const timingConstants: ConstantTable;
export declare const daBondConstants: ConstantTable;
export declare const economicsConstants: ConstantTable;
export declare const limitConstants: ConstantTable;
export declare const allowListConstants: ConstantTable;
export declare const renderAiken: (
  profile: unknown,
  template: string,
) => string;
export declare const renderTemplateExports: (template: string) => string;
