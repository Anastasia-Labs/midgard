import { clearNodeDatabase, configureNodeEndpoint } from './actions/node.js';
import { configureTxGenerator, toggleTxGenerator } from './actions/tx-generator.js';
import type { Menu } from './types.js';

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
