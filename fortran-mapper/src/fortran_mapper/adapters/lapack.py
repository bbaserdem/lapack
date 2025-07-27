"""LAPACK/BLAS specific adapter with hooks."""

import re
from typing import Dict, Any, List
from pathlib import Path

from ..hooks.base import NodeEnricher, NodeCreator
from ..core.nodes import CustomNode


class LapackNodeEnricher(NodeEnricher):
    """Node enricher for LAPACK/BLAS naming conventions."""
    
    def __init__(self):
        self.precision_map = {
            'S': 'single',
            'D': 'double', 
            'C': 'complex',
            'Z': 'double_complex'
        }
        
        self.matrix_types = {
            'GE': 'general',
            'GB': 'general_band',
            'GT': 'general_tridiagonal',
            'PO': 'positive_definite',
            'PP': 'positive_definite_packed',
            'PB': 'positive_definite_band',
            'PT': 'positive_definite_tridiagonal',
            'SY': 'symmetric',
            'SP': 'symmetric_packed',
            'SB': 'symmetric_band',
            'ST': 'symmetric_tridiagonal',
            'HE': 'hermitian',
            'HP': 'hermitian_packed',
            'HB': 'hermitian_band',
            'TR': 'triangular',
            'TP': 'triangular_packed',
            'TB': 'triangular_band',
            'OR': 'orthogonal',
            'UN': 'unitary',
            'OP': 'orthogonal_packed',
            'UP': 'unitary_packed'
        }
        
        # LAPACK/BLAS patterns for categorization
        self.lapack_patterns = {
            'blas1': re.compile(r'[SDCZ](DOT|AXPY|SCAL|COPY|SWAP|NRM2|ASUM|AMAX)', re.IGNORECASE),
            'blas2': re.compile(r'[SDCZ](GEMV|GBMV|HEMV|HBMV|HPMV|SYMV|SBMV|SPMV|TRMV|TBMV|TPMV|TRSV|TBSV|TPSV|GER|GERU|GERC|HER|HPR|HER2|HPR2|SYR|SPR|SYR2|SPR2)', re.IGNORECASE),
            'blas3': re.compile(r'[SDCZ](GEMM|SYMM|HEMM|SYRK|HERK|SYR2K|HER2K|TRMM|TRSM)', re.IGNORECASE),
            'lapack': re.compile(r'[SDCZ](GETRF|GETRS|GETRI|POTRF|POTRS|POTRI|GEQRF|GEQP3|ORGQR|ORMQR|GELQF|ORGLQ|ORMLQ|GESVD|GESDD|GBTRF|GBTRS|GTTRF|GTTRS)', re.IGNORECASE)
        }
    
    def enrich_routine(self, name: str, file_path: str, 
                      properties: Dict[str, Any]) -> Dict[str, Any]:
        """Enrich routine with LAPACK-specific properties."""
        if not name or len(name) < 2:
            return properties
        
        # Extract precision from first character
        first_char = name[0].upper()
        if first_char in self.precision_map:
            properties['precision'] = first_char.lower()
            properties['precision_name'] = self.precision_map[first_char]
            properties['operation'] = name[1:].upper()
        
        # Extract matrix type and operation details
        if len(name) >= 3:
            matrix_type = name[1:3].upper()
            if matrix_type in self.matrix_types:
                properties['matrix_type'] = matrix_type
                properties['matrix_type_name'] = self.matrix_types[matrix_type]
        
        # Add library classification
        properties['library'] = self._classify_library(file_path)
        
        # Add performance characteristics
        properties.update(self._get_performance_characteristics(name))
        
        return properties
    
    def extract_categories(self, name: str) -> List[str]:
        """Extract categories from LAPACK routine name."""
        categories = []
        
        if not name:
            return categories
        
        name_upper = name.upper()
        
        # Check BLAS/LAPACK patterns
        for category, pattern in self.lapack_patterns.items():
            if pattern.match(name):
                categories.append(category)
                break
        
        # Add operation-specific categories
        if 'TRF' in name_upper:
            categories.append('factorization')
        elif 'TRS' in name_upper:
            categories.append('solve')
        elif 'TRI' in name_upper:
            categories.append('inverse')
        elif 'QRF' in name_upper or 'QLF' in name_upper or 'LQF' in name_upper:
            categories.append('qr_decomposition')
        elif 'SVD' in name_upper:
            categories.append('svd')
        elif 'EV' in name_upper and name_upper != 'DREV':  # Eigenvalue routines
            categories.append('eigenvalue')
        
        # Matrix type categories
        if len(name) >= 3:
            matrix_type = name[1:3].upper()
            if matrix_type in self.matrix_types:
                categories.append(f'matrix_{matrix_type.lower()}')
        
        # Special utility routines
        if name_upper in ['XERBLA', 'LSAME', 'XERBLA_ARRAY']:
            categories.append('utility')
        elif name_upper.endswith('LAMCH'):
            categories.append('machine_parameters')
        elif 'ILAENV' in name_upper:
            categories.append('environment')
        
        return categories
    
    def enrich_file(self, file_path: str, properties: Dict[str, Any]) -> Dict[str, Any]:
        """Enrich file with LAPACK-specific properties."""
        path = Path(file_path)
        
        # Classify library
        properties['library'] = self._classify_library(file_path)
        
        # Add precision info based on filename
        filename = path.stem.lower()
        if filename and filename[0] in 'sdcz':
            properties['file_precision'] = filename[0]
            properties['file_precision_name'] = self.precision_map.get(filename[0].upper(), 'unknown')
        
        return properties
    
    def _classify_library(self, file_path: str) -> str:
        """Classify which library a file belongs to."""
        path_str = str(file_path).upper()
        
        if 'BLAS' in path_str:
            return 'BLAS'
        elif 'LAPACK' in path_str or 'SRC' in path_str:
            return 'LAPACK'
        elif 'CBLAS' in path_str:
            return 'CBLAS'
        elif 'LAPACKE' in path_str:
            return 'LAPACKE'
        else:
            return 'UNKNOWN'
    
    def _get_performance_characteristics(self, name: str) -> Dict[str, Any]:
        """Get performance characteristics for a routine."""
        characteristics = {}
        
        if not name:
            return characteristics
        
        name_upper = name.upper()
        
        # BLAS level determines computational complexity
        for category, pattern in self.lapack_patterns.items():
            if pattern.match(name):
                if category == 'blas1':
                    characteristics['complexity'] = 'O(n)'
                    characteristics['blas_level'] = 1
                elif category == 'blas2':
                    characteristics['complexity'] = 'O(n²)'
                    characteristics['blas_level'] = 2
                elif category == 'blas3':
                    characteristics['complexity'] = 'O(n³)'
                    characteristics['blas_level'] = 3
                break
        
        # Memory access patterns
        if any(x in name_upper for x in ['GB', 'SB', 'HB', 'TB', 'PB']):
            characteristics['memory_pattern'] = 'banded'
        elif any(x in name_upper for x in ['SP', 'HP', 'TP', 'PP']):
            characteristics['memory_pattern'] = 'packed'
        elif 'GE' in name_upper:
            characteristics['memory_pattern'] = 'full'
        
        return characteristics


class LapackNodeCreator(NodeCreator):
    """Create additional nodes for LAPACK-specific concepts."""
    
    def create_additional_nodes(self, routine_name: str, 
                               routine_properties: Dict[str, Any]) -> List[CustomNode]:
        """Create PRECISION and OPERATION nodes."""
        nodes = []
        
        # Create PRECISION node
        precision = routine_properties.get('precision')
        if precision:
            precision_node = CustomNode(
                custom_type="Precision",
                name=precision.upper(),
                symbol=precision.upper(),
                name_full=routine_properties.get('precision_name', precision),
                fortran_type=self._get_fortran_type(precision)
            )
            nodes.append(precision_node)
        
        # Create OPERATION node
        operation = routine_properties.get('operation')
        if operation and operation != routine_name:
            operation_node = CustomNode(
                custom_type="Operation", 
                name=operation,
                base_operation=operation,
                description=self._get_operation_description(operation),
                category=self._get_operation_category(operation)
            )
            nodes.append(operation_node)
        
        # Create MATRIX_TYPE node
        matrix_type = routine_properties.get('matrix_type')
        if matrix_type:
            matrix_node = CustomNode(
                custom_type="MatrixType",
                name=matrix_type,
                code=matrix_type,
                description=routine_properties.get('matrix_type_name', matrix_type),
                storage_scheme=self._get_storage_scheme(matrix_type)
            )
            nodes.append(matrix_node)
        
        return nodes
    
    def _get_fortran_type(self, precision: str) -> str:
        """Get Fortran type for precision."""
        type_map = {
            's': 'REAL',
            'd': 'DOUBLE PRECISION', 
            'c': 'COMPLEX',
            'z': 'DOUBLE COMPLEX'
        }
        return type_map.get(precision.lower(), 'UNKNOWN')
    
    def _get_operation_description(self, operation: str) -> str:
        """Get description for operation."""
        descriptions = {
            'GEMM': 'General matrix-matrix multiply',
            'GEMV': 'General matrix-vector multiply',
            'GETRF': 'LU factorization',
            'GETRS': 'Solve using LU factorization',
            'POTRF': 'Cholesky factorization',
            'POTRS': 'Solve using Cholesky factorization',
            'GEQRF': 'QR factorization',
            'GESVD': 'Singular value decomposition',
            'GESDD': 'SVD using divide-and-conquer',
            'AXPY': 'Y = alpha*X + Y',
            'DOT': 'Dot product',
            'NRM2': 'Euclidean norm',
            'SCAL': 'Scale vector',
            'COPY': 'Copy vector',
            'SWAP': 'Swap vectors'
        }
        return descriptions.get(operation, f'{operation} operation')
    
    def _get_operation_category(self, operation: str) -> str:
        """Get category for operation."""
        if any(x in operation for x in ['TRF']):
            return 'factorization'
        elif any(x in operation for x in ['TRS']):
            return 'linear_solve'
        elif any(x in operation for x in ['SVD']):
            return 'decomposition'
        elif any(x in operation for x in ['MM', 'MV']):
            return 'multiplication'
        elif operation in ['DOT', 'NRM2', 'ASUM']:
            return 'reduction'
        elif operation in ['AXPY', 'SCAL', 'COPY', 'SWAP']:
            return 'vector_operation'
        else:
            return 'other'
    
    def _get_storage_scheme(self, matrix_type: str) -> str:
        """Get storage scheme for matrix type."""
        if matrix_type.endswith('P'):
            return 'packed'
        elif matrix_type.endswith('B'):
            return 'banded'
        elif matrix_type in ['GE', 'SY', 'HE', 'TR']:
            return 'full'
        else:
            return 'special'