import type { MidgardConfig } from '../../types/config.js';

export type InteractiveContext = {
  config: MidgardConfig;
};

export type ActionResult = {
  success: boolean;
  message: string;
};

export type Action = {
  name: string;
  description: string;
  execute: (context: InteractiveContext) => Promise<ActionResult>;
};

export type MenuSection = {
  name: string;
  description: string;
  actions: Action[];
};

export type Menu = {
  sections: MenuSection[];
};
