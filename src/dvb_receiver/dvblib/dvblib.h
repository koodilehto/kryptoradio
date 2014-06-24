/*! \file dvblib.h
 * \brief A very simple DVB library.
 * \author Tuomas Virtanen
 * \copyright MIT
 * \date 2014
*/

#ifndef _DVBLIB_H
#define _DVBLIB_H

#include <inttypes.h>

enum DVB_TYPE {
    DVB_TYPE_TERRESTIAL, ///< Device is DVB-T
    DVB_TYPE_SATELLITE, ///< Device is DVB-S
    DVB_TYPE_CABLE, ///< Device is DVB-C
    DVB_TYPE_COUNT, ///< Number of types
};

enum DVB_STATUS {
    DVB_STATUS_HAS_SIGNAL = 0x1, ///< Some sort of signal was found
    DVB_STATUS_HAS_CARRIER = 0x2, ///< DVB carrier found
    DVB_STATUS_HAS_VITERBI = 0x4, ///< FEC is stable
    DVB_STATUS_HAS_SYNC = 0x8, ///< Sync bytes found
    DVB_STATUS_HAS_LOCK = 0x10, ///< Signal lock achieved
    DVB_STATUS_TIMEDOUT = 0x20, ///< No signal lock within the last ~2 seconds
    DVB_STATUS_REINIT = 0x40, ///< Frontend was reinitialized, reset recommended
};

enum DVB_STREAM_TYPE {
    DVB_STREAM_VIDEO, ///< Video
    DVB_STREAM_AUDIO, ///< Audio
    DVB_STREAM_TELETEXT, ///< Teletext
    DVB_STREAM_SUBTITLE, ///< Subtitles
    DVB_STREAM_PCR, ///< System time counter
    DVB_STREAM_OTHER, ///< Some other (unknown) data
    DVB_STREAM_TYPE_COUNT, ///< Number of stream types
};

typedef struct {
    int fd_frontend; ///< Frontend handle
    int fd_demuxer; ///< Demuxer handle
    char error[256]; ///< Errormessage buffer (0 terminated)
    char name[128]; ///< Device name (0 terminated)
    int type; ///< Device type (se DVB_TYPE)
} dvb_device;

/*! \brief Opens a DVB device for operations
 * 
 * This function opens a DVB device for reading and writing. Both the frontend
 * and demuxer are opened. The devices opened will be /dev/dvb/adapter<dev_id>/frontend<frontend_id>
 * and /dev/dvb/adapter<dev_id>/demux<demuxer_id>.
 *
 * On success, value 0 is returned. After a successful dvb_open(), the device should be closed
 * by using dvb_close(). On error, value 1 is returned and dvb_get_error() returns the error
 * message. The dvb_device struct will be invalid and should not be used.
 *
 * \param dev Allocated device struct
 * \param dev_id Device ID to open
 * \param frontend_id Frontend ID to open
 * \param demuxer_id Demuxer ID to open
 * \return Success value
 */
int dvb_open(dvb_device *dev, int dev_id, int frontend_id, int demuxer_id);

/*! \brief Closes an open DVB device
 * 
 * This function closes an open DVB device. After this, the device struct will be invalid
 * and should not be used.
 *
 * \param dev Opened device struct
 */
void dvb_close(dvb_device *dev);

/*! \brief Tunes the DVB device to some frequency
 * 
 * This function tunes the DVB frontend to a given frequency. Note! At the moment, only DVB-T is
 * properly supported. DVB-C and DVB-S will likely NOT work at all. All values (except bandwidth
 * and hierarchy, which are 8MHz and None) will be automatically found if the device is good enough.
 * If not, then you're out of luck :)
 *
 * Retuning can be done by just calling this function again.
 *
 * On success, value 0 is returned and the device should be tuned.
 * On error, value 1 is returned and dvb_get_error() returns the error message.
 *
 * \param dev Opened device struct
 * \param frequency Frequency to tune to
 * \return Success value
 */
int dvb_tune(dvb_device *dev, size_t frequency);

/*! \brief Gets the status of the device
 * 
 * This function finds the status of the device. The status message is a combination of the following:
 * 
 * - DVB_STATUS_HAS_SIGNAL
 * - DVB_STATUS_HAS_CARRIER
 * - DVB_STATUS_HAS_VITERBI
 * - DVB_STATUS_HAS_SYNC
 * - DVB_STATUS_HAS_LOCK
 * - DVB_STATUS_TIMEDOUT
 * - DVB_STATUS_REINIT
 *
 * \code{.c}
 * uint32_t status;
 * dvb_get_status(&dev, &status);
 * if(status & DVB_STATUS_HAS_LOCK) {
 *     printf("Lock achieved!\n");
 *     ...  
 * }
 * \endcode
 *
 * On success, value 0 is returned and the value variable will contain valid information.
 * On error, value 1 is returned, and dvb_get_error() returns the error message.
 *
 * \param dev Opened device struct
 * \param value Status value
 * \return Success value
 */
int dvb_get_status(dvb_device *dev, uint32_t *value);

/*! \brief Gets the bit error rate of the device
 * 
 * This function finds the bit error rate of the device.
 *
 * On success, value 0 is returned and the value variable will contain valid information.
 * On error, value 1 is returned, and dvb_get_error() returns the error message.
 *
 * \param dev Opened device struct
 * \param value Status value
 * \return Success value
 */
int dvb_get_ber(dvb_device *dev, uint32_t *value);

/*! \brief Gets the signal to noise ratio (SNR) of the device
 * 
 * This function finds the signal to noise ratio (SNR) of the device.
 *
 * On success, value 0 is returned and the value variable will contain valid information.
 * On error, value 1 is returned, and dvb_get_error() returns the error message.
 *
 * \param dev Opened device struct
 * \param value Status value
 * \return Success value
 */
int dvb_get_snr(dvb_device *dev, int16_t *value);

/*! \brief Gets the signal strength of the device
 * 
 * This function finds the signal strength of the device.
 *
 * On success, value 0 is returned and the value variable will contain valid information.
 * On error, value 1 is returned, and dvb_get_error() returns the error message.
 *
 * \param dev Opened device struct
 * \param value Status value
 * \return Success value
 */
int dvb_get_signal_strength(dvb_device *dev, int16_t *value);

/*! \brief Gets the uncorrected block count of the device
 * 
 * This function finds the uncorrected block count of the device.
 *
 * On success, value 0 is returned and the value variable will contain valid information.
 * On error, value 1 is returned, and dvb_get_error() returns the error message.
 *
 * \param dev Opened device struct
 * \param value Status value
 * \return Success value
 */
int dvb_get_uncorrected_blocks(dvb_device *dev, uint32_t *value);

/*! \brief Returns the last error message
 * 
 * This function returns the last error message generated by the library.
 * Note! This should ONLY be called after a clear error signal from one of the functions.
 * The error message WILL NOT be cleared after a call to this function.
 *
 * \param dev Opened device struct
 * \return Error string
 */
const char* dvb_get_error(dvb_device *dev);

/*! \brief Returns a descriptive string for type ID
 * 
 * This function returns a descriptive type information string for the given device.
 *
 * \param dev Opened device struct
 * \return Type string
 */
const char* dvb_get_type_str(dvb_device *dev);

/*! \brief Returns a descriptive string for the stream PES filter type
 * 
 * This function returns a descriptive name string for the given stream PES type.
 *
 * \param stream_type Stream type ID
 * \return Name string
 */
const char* dvb_get_stream_type_str(int stream_type);

/*! \brief Set demuxer internal buffer size
 * 
 * This function sets the internal buffer size for the demuxer. If this is not called,
 * a buffer size of 2*4096 is used.
 *
 * On success, value 0 is returned and the new buffer size is in use.
 * On error, value 1 is returned and dvb_get_error() returns the error message.
 *
 * \param dev Opened device struct
 * \param size New buffer size
 * \return Success value
 */
int dvb_set_buffer_size(dvb_device *dev, unsigned long size);

/*! \brief Initializes a PES filtering and streaming
 * 
 * This function sets the demuxer to extract certain types of data from the MPEG transport
 * stream with a PES filter. The filter may be one of the following:
 * 
 * - DVB_STREAM_VIDEO,
 * - DVB_STREAM_AUDIO,
 * - DVB_STREAM_TELETEXT,
 * - DVB_STREAM_SUBTITLE,
 * - DVB_STREAM_PCR,
 * - DVB_STREAM_OTHER
 *
 * User should make sure that the given PID is valid. Otherwise reading the device may
 * block indefinetely. PID values can be found by using eg. dvbsnoop.
 *
 * On success, value 0 is returned and the data can be read by calling dvb_read_stream().
 * On error, value 1 is returned and dvb_get_error() returns the error message.
 *
 * \param dev Opened device struct
 * \param pid PID key
 * \param pes_type PES type (see above)
 * \return Success value
 */
int dvb_init_pes_stream(dvb_device *dev, int pid, int pes_type);

/*! \brief Initializes a section filtering and streaming
 * 
 * This function sets the demuxer to extract section data from the MPEG transport stream.
 *
 * User should make sure that the given PID is valid. Otherwise reading the device may
 * block indefinetely. PID values can be found by using eg. dvbsnoop.
 *
 * On success, value 0 is returned and the data can be read by calling dvb_read_stream().
 * On error, value 1 is returned and dvb_get_error() returns the error message.
 *
 * \param dev Opened device struct
 * \param pid PID key
 * \return Success value
 */
int dvb_init_section_stream(dvb_device *dev, int pid);

/*! \brief Reads the stream for data
 * 
 * This function reads the demuxer for data. The demuxer should have been initialized with
 * dvb_init_pes_stream() or dvb_init_section_stream() before this.
 *
 * This function will block until data is available. Hardware buffer is usually 8192 bytes,
 * so any read calls with requests bigger than that _WILL_ block. Note that you may also set
 * the buffer yourself using dvb_set_buffer_size(). 
 * 
 * Note that if you're not reading the data from the hardware fast enough, you will start getting overflow
 * or timeout errors. In this case you should either read faster, read larger blocks, or grow your hw buffer
 * using dvb_set_buffer_size().
 *
 * On success, the value will be higher than 0, and contains the amount of data read from the device.
 * On EOF, value 0 is returned. 
 * On general error (bad params and such), value -1 is returned.
 * On hw buffer overflow, -2 is returned.
 * On hw timeout, -3 is returned.
 * In all error cases, dvb_get_error() returns the full error message
 *
 * \param dev Opened device struct
 * \param buffer Buffer to read into. Make sure it's big enough.
 * \param size Read request size
 * \return Success value or amount of data read
 */
size_t dvb_read_stream(dvb_device *dev, void *buffer, size_t size);

/*! \brief Stops the streaming from demuxer
 * 
 * This function stops any and all demuxer operations, eg. streaming. No
 * dvb_read_stream() calls should be done after this before a call to 
 * dvb_init_section_stream() or dvb_init_pes_stream().
 *
 * On success, value 0 is returned.
 * On error, value 1 is returned and dvb_get_error() returns the error message.
 *
 * \param dev Opened device struct
 * \return Success value
 */
int dvb_stop_stream(dvb_device *dev);

#endif // _DVBLIB_H
