import { clearNodeDatabase, configureNodeEndpoint } from './actions/node.js';
import { configureTxGenerator, toggleTxGenerator } from './actions/tx-generator.js';
import type { Menu } from './types.js';

/**
 * Static menu definition for the manager's interactive mode.
 *
 * Keeping the menu declarative makes it easy to see which operational areas the
 * CLI exposes and how those areas are grouped without stepping through the
 * command-loop implementation.
 */
export const menu: Menu = {
  sections: [
    {
      name: 'Node Operations',
      description: 'Configure endpoint and manage node mempool',
      actions: [configureNodeEndpoint, clearNodeDatabase],
    },
    {
      name: 'Transaction Generator',
      description: 'Configure and manage transaction generation',
      actions: [configureTxGenerator, toggleTxGenerator],
    },
  ],
};
