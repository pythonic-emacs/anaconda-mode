"""
Autocompletion server for ``anaconda-mode`` Emacs plugin.

See https://github.com/proofit404/anaconda-mode for more details.
"""

from setuptools import setup

setup(
    name='anaconda_mode',
    version='0.1.9',
    url='https://github.com/proofit404/anaconda-mode',
    description='Code navigation, documentation lookup and '
    'completion for Python.',
    long_description=__doc__,
    platforms='any',
    license='GPL3',
    author='Artem Malyshev',
    author_email='proofit404@gmail.com',
    maintainer='Artem Malyshev',
    maintainer_email='proofit404@gmail.com',
    py_modules=['anaconda_mode'],
    zip_safe=False,
    install_requires=[
        'jedi>=0.11',
        'service_factory>=0.1.5',
    ],
    classifiers=[
        'Development Status :: 4 - Beta',
        'Environment :: Console',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: GNU General Public License v3 (GPLv3)',
        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 2.6',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.2',
        'Programming Language :: Python :: 3.3',
        'Programming Language :: Python :: 3.4',
        'Programming Language :: Python :: 3.5',
        'Programming Language :: Python :: 3.6',
        'Topic :: Text Editors',
        'Topic :: Text Editors :: Emacs',
    ],
)
