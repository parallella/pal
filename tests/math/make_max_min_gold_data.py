# -*- coding: utf-8 -*-

# Create the comma separated value files for the test data for the
# min and max functions

import numpy

NUMBER_OF_VALUES = 100  # Consistent with the other tests
OUTPUT_MIN_FILE = "./gold/p_min_f32.dat"
OUTPUT_MAX_FILE = "./gold/p_max_f32.dat"

gold_datatype = [('input_a', numpy.float32),
    ('input_b', numpy.float32),
    ('reference', numpy.float32),
    ('gold', numpy.float32)
    ]
gold_data = numpy.zeros((NUMBER_OF_VALUES,), dtype=gold_datatype)

gold_data['input_a'] = numpy.random.random(NUMBER_OF_VALUES)

# For scalar output, only the first array entry is filled.
gold_data['gold'][0] = numpy.max(gold_data['input_a'])

numpy.savetxt(OUTPUT_MAX_FILE, gold_data, delimiter=',',
              fmt=("%6.5f", "%6.5f", "%6.5f", "%6.5f"))

gold_data['gold'][0] = numpy.min(gold_data['input_a'])

numpy.savetxt(OUTPUT_MIN_FILE, gold_data, delimiter=',',
              fmt=("%6.5f", "%6.5f", "%6.5f", "%6.5f"))
