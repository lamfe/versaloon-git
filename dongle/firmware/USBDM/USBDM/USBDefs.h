#ifndef _USBDefs_H_
#define _USBDefs_H_

#include "Common.h"

/*----------------------------------------------------------------------------
** device descriptor
*/
typedef struct {
   U8           bLength;             /* Size of this Descriptor in Bytes */
   U8           bDescriptorType;     /* Descriptor Type (=1) */
   U16u         bcdUSB;              /* USB Spec Release Number in BCD */
   U8           bDeviceClass;        /* Device Class Code */
   U8           bDeviceSubClass;     /* Device Subclass Code */      
   U8           bDeviceProtocol;     /* Device Protocol Code */
   U8           bMaxPacketSize0;     /* Maximum Packet Size for EP0 */
   U16u         idVendor;            /* Vendor ID */ 
   U16u         idProduct;           /* Product ID */
   U16u         bcdDevice;           /* Device Release Number in BCD */
   U8           iManufacturer;       /* Index of String Desc for Manufacturer */
   U8           iProduct;            /* Index of String Desc for Product */
   U8           iSerialNumber;       /* Index of String Desc for SerNo */
   U8           bNumConfigurations;  /* Number of possible Configurations */
} DeviceDescriptor;                

/*----------------------------------------------------------------------------
** Standard Configuration Descriptor
*/
typedef struct {
   U8           bLength;             /* Size of this Descriptor in Bytes */
   U8           bDescriptorType;     /* Descriptor Type (=2) */
   U16u         wTotalLength;        /* Total Length of Data for this Configuration */
   U8           bNumInterfaces;      /* No of Interfaces supported by this Configuration */
   U8           bConfigurationValue; /* Designator Value for this Configuration */
   U8           iConfiguration;      /* Index of String Desc for this Configuration */
   U8           bmAttributes;        /* Configuration Characteristics */
   U8           bMaxPower;           /* Max. Power Consumption in this Configuration (in 2mA steps) */
} ConfigurationDescriptor;

/*----------------------------------------------------------------------------
** Standard Interface Descriptor
*/
typedef struct {
   U8           bLength;             /* Size of this Descriptor in Bytes */
   U8           bDescriptorType;     /* Descriptor Type (=4)             */
   U8           bInterfaceNumber;    /* Number of this Interface (0..)   */
   U8           bAlternateSetting;   /* Alternative for this Interface (if any) */
   U8           bNumEndpoints;       /* No of EPs used by this IF (excl. EP0) */
   U8           bInterfaceClass;     /* Interface Class Code */
   U8           bInterfaceSubClass;  /* Interface Subclass Code */
   U8           bInterfaceProtocol;  /* Interface Protocol Code */
   U8           iInterface;          /* Index of String Desc for this Interface */
} InterfaceDescriptor;

/*----------------------------------------------------------------------------
** Standard Endpoint Descriptor
*/
typedef struct {
   U8           bLength;             /* Size of this Descriptor in Bytes */
   U8           bDescriptorType;     /* Descriptor Type (=5) */
   U8           bEndpointAddress;    /* Endpoint Address (Number + Direction) */
   U8           bmAttributes;        /* Endpoint Attributes (Transfer Type) */
   U16u         wMaxPacketSize;      /* Max. Endpoint Packet Size */
   U8           bInterval;           /* Polling Interval (Interrupt) in ms */
} EndpointDescriptor;

/*--------------------------------------------------------------------------------
** Structure of Setup Packet sent during SETUP Stage of Standard Device Requests
*/
typedef struct {
   U8           bmRequestType;       /* Characteristics (Direction,Type,Recipient) */
   U8           bRequest;            /* Standard Request Code */
   U16u         wValue;              /* Value Field */
   U16u         wIndex;              /* Index or Offset Field */
   U16u         wLength;             /* Number of Bytes to transfer (Data Stage) */
} SetupPacket;

/*--------------------------------------------------------------------------------
** Structure of Device Qualifier Descriptor
*/
typedef struct {
   U8           bLength;             /* Size of this Descriptor in Bytes */
   U8           bDescriptorType;     /* Descriptor Type (=6) */
   U16u         bcdUSB;              /* USB Spec Release Number in BCD */
   U8           bDeviceClass;        /* Device Class Code */
   U8           bDeviceSubClass;     /* Device Subclass Code */      
   U8           bDeviceProtocol;     /* Device Protocol Code */
   U8           bMaxPacketSize0;     /* Maximum Packet Size for EP0 */
   U8           bNumConfigurations;  /* Number of possible Configurations */
   U8           bReserved;           /* Reserved */
} DeviceQualifierDescriptor;

/*----------------------------------------------------------------------------
** USB Status Codes
*/
#define US_ATTACHED             0x00
#define US_POWERED              0x01
#define US_DEFAULT              0x02
#define US_ADDRESSED            0x03
#define US_CONFIGURED           0x04
#define US_SUSPENDED            0x80

/*----------------------------------------------------------------------------
** USB Standard Device Request Codes (bRequest)
*/
#define GET_STATUS              0x00
#define CLEAR_FEATURE           0x01
#define SET_FEATURE             0x03
#define SET_ADDRESS             0x05
#define GET_DESCRIPTOR          0x06
#define SET_DESCRIPTOR          0x07
#define GET_CONFIGURATION       0x08
#define SET_CONFIGURATION       0x09
#define GET_INTERFACE           0x0a
#define SET_INTERFACE           0x0b
#define SYNCH_FRAME             0x0c

#define IS_VENDOR_REQ(x)     (((x)&0x60) == 0x40)

/*----------------------------------------------------------------------------
** Descriptor Types
*/
#define DT_DEVICE                   1
#define DT_CONFIGURATION            2
#define DT_STRING                   3
#define DT_INTERFACE                4
#define DT_ENDPOINT                 5
#define DT_DEVICEQUALIFIER          6
#define DT_OTHERSPEEDCONFIGURATION  7
#define DT_INTERFACEPOWER           8

/*----------------------------------------------------------------------------
** USB Tokens
*/
#define SOFToken     (0x5) // Start of Frame token
#define SETUPToken   (0xD) // Setup token
#define OUTToken     (0x1) // Out token
#define INToken      (0x9) // In token
#define DATA0Token   (0x3) // Data 0
#define DATA1Token   (0xB) // Data 1
#define DATA2Token   (0x7) // Data 2
#define MDATAToken   (0xF) // M data
#define ACKToken     (0x2) // Acknowledge
#define NAKToken     (0xA) // Negative Acknowledge
#define NYETToken    (0x6) // No Response Yet
#define STALLToken   (0xE) // Stall
#define PREToken     (0xC) // Preamble

/*----------------------------------------------------------------------------
** Feature selector values (for Clear/Set feature)
*/
#define ENDPOINT_HALT         (0x00)
#define DEVICE_REMOTE_WAKEUP  (0x01)
#define TEST_MODE             (0x02)

/*----------------------------------------------------------------------------
** bmRequest types
*/
#define RT_DEVICE          (0x00)
#define RT_INTERFACE       (0x01)
#define RT_ENDPOINT        (0x02)

#endif /* _USBDefs_H_  */
