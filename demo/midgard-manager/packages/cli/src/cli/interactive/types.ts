import type { MidgardConfig } from '../../types/config.js';

/**
 * Context object passed to interactive actions.
 */
export type InteractiveContext = {
  config: MidgardConfig;
};

/**
 * Standard result returned by an interactive action.
 */
export type ActionResult = {
  success: boolean;
  message: string;
};

/**
 * One executable interactive action displayed in a menu section.
 */
export type Action = {
  name: string;
  description: string;
  execute: (context: InteractiveContext) => Promise<ActionResult>;
};

/**
 * Group of actions shown under one interactive menu section.
 */
export type MenuSection = {
  name: string;
  description: string;
  actions: Action[];
};

/**
 * Full interactive menu tree.
 */
export type Menu = {
  sections: MenuSection[];
};
