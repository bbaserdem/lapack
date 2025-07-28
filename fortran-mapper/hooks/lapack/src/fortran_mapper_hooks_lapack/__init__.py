"""
LAPACK-specific hooks for fortran-mapper.

This package provides LAPACK-specific functionality for the fortran-mapper
library, including node enrichers and creators that understand LAPACK
naming conventions and computational categories.
"""

__version__ = "0.1.0"

# Import main hook classes
from .enricher import LapackNodeEnricher
from .creator import LapackNodeCreator

__all__ = [
    "LapackNodeEnricher",
    "LapackNodeCreator",
]
