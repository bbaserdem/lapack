"""LAPACK/BLAS specific node creator."""

from typing import Dict, Any, List

from fortran_mapper.hooks.base import NodeCreator
from fortran_mapper.core.nodes import CustomNode


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