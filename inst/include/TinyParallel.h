#ifndef __RAVETOOLS_PARALLEL__
#define __RAVETOOLS_PARALLEL__

// TinyThread implementation
#include "TinyParallel/TinyThread.h"

#include "TinyParallel/RVector.h"

namespace TinyParallel {

inline void parallelFor(std::size_t begin,
                        std::size_t end,
                        Worker& worker,
                        std::size_t grainSize = 1,
                        int numThreads = -1)
{
   grainSize = resolveValue("RAVETOOLS_GRAIN_SIZE", grainSize, 1u);
   numThreads = resolveValue("RAVETOOLS_NUM_THREADS", numThreads, -1);

   ttParallelFor(begin, end, worker, grainSize);
}

template <typename Reducer>
inline void parallelReduce(std::size_t begin,
                           std::size_t end,
                           Reducer& reducer,
                           std::size_t grainSize = 1,
                           int numThreads = -1)
{
   grainSize = resolveValue("RAVETOOLS_GRAIN_SIZE", grainSize, 1);
   numThreads = resolveValue("RAVETOOLS_NUM_THREADS", numThreads, -1);

   ttParallelReduce(begin, end, reducer, grainSize);
}

} // end namespace TinyParallel

#endif // __RAVETOOLS_PARALLEL__
