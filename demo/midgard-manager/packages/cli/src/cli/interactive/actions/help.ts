import chalk from 'chalk';

import { MidgardError } from '../../../utils/errors.js';
import type { Action } from '../types.js';

/**
 * Interactive action that prints the extended help/usage guide.
 */
export const viewHelp: Action = {
  name: 'View Help',
  description: 'Display help information and usage tips',
  execute: async () => {
    try {
      console.log('\n');
      console.log(chalk.bold('📘 Midgard Manager - Advanced Guide'));
      console.log(chalk.dim('─'.repeat(70)));

      // Transaction Types Section
      console.log(chalk.bold('\n🔄 Understanding Transaction Types:\n'));

      console.log(chalk.bold('One-to-One Transactions:'));
      console.log(chalk.dim('  • Use case: Simple payments between two parties'));
      console.log(
        chalk.dim('  • Characteristics: Lower computational overhead, faster verification')
      );
      console.log(chalk.dim('  • Ideal for: High-volume testing, performance benchmarks'));
      console.log(chalk.dim('  • Technical details: Single input, single output transactions'));

      console.log(chalk.bold('\nMulti-Output Transactions:'));
      console.log(
        chalk.dim('  • Use case: Distributing funds to multiple recipients simultaneously')
      );
      console.log(
        chalk.dim('  • Characteristics: Higher computational overhead, more complex validation')
      );
      console.log(
        chalk.dim('  • Ideal for: Testing mempool behavior, complex transaction handling')
      );
      console.log(chalk.dim('  • Technical details: Single input, multiple output UTXOs'));

      console.log(chalk.bold('\nMixed Transactions:'));
      console.log(chalk.dim('  • Use case: Realistic network simulation'));
      console.log(chalk.dim('  • Characteristics: Varied transaction sizes and complexity'));
      console.log(chalk.dim('  • Ideal for: Stress testing, real-world usage simulation'));
      console.log(
        chalk.dim(
          '  • Note: Ratio setting determines the proportion of simple vs. complex transactions'
        )
      );

      // System Configuration Section
      console.log(chalk.bold('\n⚙️ System Configuration Recommendations:\n'));

      console.log(chalk.bold('Performance Optimization:'));
      console.log(chalk.dim('  • For high throughput: Use smaller batches with high concurrency'));
      console.log(
        chalk.dim('  • For stability testing: Use larger batches with lower concurrency')
      );
      console.log(chalk.dim('  • Memory consideration: Large batches consume more memory'));

      console.log(chalk.bold('\nNode Connection:'));
      console.log(
        chalk.dim('  • Ensure your node endpoint is correctly configured and the node is running')
      );
      console.log(chalk.dim('  • Local testing: Use http://localhost:{port} for local node'));
      console.log(chalk.dim('  • Remote connections: Ensure proper authentication if required'));

      // Troubleshooting Section
      console.log(chalk.bold('\n🔧 Troubleshooting Common Issues:\n'));

      console.log(chalk.bold('Transaction Generation Issues:'));
      console.log(
        chalk.dim(
          '  • Insufficient funds: Ensure wallet has enough funds to cover all transactions'
        )
      );
      console.log(
        chalk.dim('  • Connection errors: Verify node is running and endpoint is correct')
      );
      console.log(chalk.dim('  • High failure rates: Reduce concurrency or increase interval'));

      console.log(chalk.bold('\nPerformance Issues:'));
      console.log(chalk.dim('  • System slowing down: Reduce batch size or increase interval'));
      console.log(chalk.dim('  • Node not keeping up: Check node logs for bottlenecks'));
      console.log(chalk.dim('  • High memory usage: Decrease batch size or concurrency'));

      // Feature Status Section
      console.log(chalk.bold('\n🚧 Feature Development Status:\n'));

      console.log(chalk.bold('Currently Implemented:'));
      console.log(chalk.dim('  • Basic and advanced transaction generation'));
      console.log(chalk.dim('  • Transaction monitoring and statistics'));
      console.log(chalk.dim('  • Wallet management and configuration'));

      console.log(chalk.bold('\nPlanned Features:'));
      console.log(chalk.dim('  • Mempool monitoring and management (in development)'));
      console.log(chalk.dim('  • Advanced transaction analytics'));
      console.log(chalk.dim('  • Performance benchmarking tools'));

      return {
        success: true,
        message: 'Detailed help information displayed',
      };
    } catch (error: unknown) {
      throw MidgardError.config(`Error displaying help information: ${error}`);
    }
  },
};
